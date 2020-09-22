<?php
namespace kak\RegexParserAST;


/**
 * Port lib https://github.com/jviereck/regjsparser to php
 *
 * Class RegexParserAST
 */
class RegexParserAST
{

    private $source;
    private $pos = 0;

    private $hasUnicodeFlag = false;
    private $firstIteration = true;
    private $namedGroups = false;
    private $unicodePropertyEscape = false;
    private $closedCaptureCounter = 0;
    private $backrefDenied = [];


    private const TYPE_DISJUNCTION = 'disjunction';
    private const TYPE_ALTERNATIVE = 'alternative';
    private const TYPE_GROUP = 'group';
    private const TYPE_NORMAL = 'normal';
    private const TYPE_ANCHOR = 'anchor';
    private const TYPE_UNICODE_PROPERTY_ESCAPE = 'unicodePropertyEscape';
    private const TYPE_DOT = 'dot';
    private const TYPE_VALUE = 'value';

    private const KIND_OCTAL = 'octal';
    private const KIND_SYMBOL = 'symbol';



    /**
     * todo move all const here
     */


    /**
     * ParserAST constructor.
     * @param string $source
     * @param array $options = [
     *      'flags' => 'ui'
     * ];
     */
    public function __construct(string $source, array $options = [])
    {
        if ((string)$source === '') {
            $source = '(?:)';
        }
        $this->source = $source;

        $flags = $options['flags'] ?? '';
        if (strpos($flags, 'u') !== false) {
            $this->hasUnicodeFlag = true;
        }

        $this->namedGroups = $options['namedGroups'] ?? false;
        $this->unicodePropertyEscape = $options['unicodePropertyEscape'] ?? false;
    }

    public function parse()
    {
        return $this->parseDisjunction();
    }


    private function makeThrow(string $message, int $from, int $to)
    {
        return new \Exception(
            sprintf('%s source from %s to %s', $message, $from, $to)
        );
    }

    private function lookahead(): string
    {
        return $this->source[$this->pos] ?? '';
    }

    private function incr(int $amount = null)
    {
        if ($amount === null) {
            $amount = 1;
        }
        $result = substr($this->source, $this->pos, $this->pos + $amount);
        $this->pos += $amount;
        return $result;
    }

    private function skip(string $value)
    {
        $from = $this->pos;
        if (!$this->match($value)) {
            $this->makeThrow(sprintf('character %s', $value), $from, $this->pos);
        }
    }

    private function matchReg(string $regExpString)
    {
        $sub = substr($this->source, $this->pos);
        $match = [];

        if (preg_match($regExpString, $sub, $match)) {
            $match['range'] = [];
            $match['range'][] = $this->pos;
            $this->incr(strlen($match[0]));
            $match['range'][] = $this->pos;
        }
        return $match;
    }

    private function match(string $value)
    {
        if ($this->current($value)) {
            return $this->incr(strlen($value));
        }

        return null;
    }

    private function current(string $value): bool
    {
        return strpos($this->source, $value, $this->pos) === $this->pos;
    }


    private function next(string $value): bool
    {
        return $this->source[$this->pos + 1] === $value;
    }

    private function parseDisjunction(): array
    {
        // Disjunction ::
        //      Alternative
        //      Alternative | Disjunction

        $alternatives = [];
        $from = $this->pos;

        $alternatives[] = $this->parseAlternative();

        while ($this->match('|')) {
            $alternatives[] = $this->parseAlternative();
        }

        if (count($alternatives) === 1) {
            return $alternatives[0];
        }

        return $this->createDisjunction($alternatives, $from, $this->pos);
    }


    private function createDisjunction(array $alternatives, int $from, int $to): array
    {
        $result = $this->createTypeAndRange(self::TYPE_DISJUNCTION, $from, $to);
        $result['body'] = $alternatives;
        return $this->addendRaw($result);
    }


    private function parseAlternative(): array
    {
        $terms = [];
        $from = $this->pos;

        while ($term = $this->parseTerm()) {
            $terms[] = $term;
        }

        if (count($terms) === 1) {
            return $terms[0];
        }
        return $this->createAlternative($terms, $from, $this->pos);
    }



    private function createAlternative($terms, $from, $to)
    {
        $result = $this->createTypeAndRange(self::TYPE_ALTERNATIVE, $from, $to);
        $result['body'] = $terms;
        return $this->addendRaw($result);
    }


    private function parseAnchor()
    {
        // Anchor ::
        //      ^
        //      $
        //      \ b
        //      \ B
        //      ( ? = Disjunction )
        //      ( ? ! Disjunction )

        if ($this->match('^')) {
            return $this->createAnchor('start', $this->pos - 1, $this->pos);
        }
        if ($this->match('$')) {
            return $this->createAnchor('end', $this->pos - 1, $this->pos);
        }
        if ($this->match("\\b")) {
            return $this->createAnchor('boundary', $this->pos - 2, $this->pos);
        }
        if ($this->match("\\B")) {
            return $this->createAnchor('not-boundary', $this->pos - 2, $this->pos);
        }
        if ($result = $this->parseGroup('(?=', 'lookahead')) {
            return $result;
        }
        if ($result = $this->parseGroup('(?!', 'negativeLookahead')) {
            return $result;
        }

        return false;
    }

    private function parseGroup($match, $type)
    {
        $form = $this->pos;
        if ($this->match($match)) {
            return $this->finishGroup($type, $form);
        }
        return false;
    }

    private function finishGroup($type, $from)
    {
        $body = $this->parseDisjunction();
        if (!$body) {
            $this->makeThrow('Expected disjunction', $from, $this->pos);
        }
        $this->skip(')');

        $group = $this->createGroup($type, $this->flattenBody($body), $from, $this->pos);


        if ($type == 'normal') {
            // Keep track of the number of closed groups. This is required for
            // parseDecimalEscape(). In case the string is parsed a second time the
            // value already holds the total count and no incrementation is required.
            if ($this->firstIteration) {
                $this->closedCaptureCounter++;
            }
        }


        return $group;
    }

    private function flattenBody($body)
    {
        if ($body['type'] === self::TYPE_ALTERNATIVE) {
            return $body['body'];
        }
        return [$body];
    }



    private function createGroup($behavior, $disjunction, $from, $to)
    {
        $result = $this->createTypeAndRange(self::TYPE_GROUP, $from, $to);
        $result['behavior'] = $behavior;
        $result['body'] = $disjunction;
        return $this->addendRaw($result);
    }



    private function createAnchor($kind, $from, $to)
    {
        $result = $this->createTypeAndRange(self::TYPE_ANCHOR, $from, $to);
        $result['kind'] = $kind;
        return $this->addendRaw($result);
    }

    private function parseAtomAndExtendedAtom()
    {
        // Parsing Atom and ExtendedAtom together due to redundancy.
        // ExtendedAtom is defined in Apendix B of the ECMA-262 standard.
        //
        // SEE: https://www.ecma-international.org/ecma-262/10.0/index.html#prod-annexB-ExtendedPatternCharacter
        //
        // Atom ::
        //      PatternCharacter
        //      .
        //      \ AtomEscape
        //      CharacterClass
        //      ( GroupSpecifier Disjunction )
        //      ( ? : Disjunction )
        // ExtendedAtom ::
        //      ExtendedPatternCharacter
        // ExtendedPatternCharacter ::
        //      SourceCharacter but not one of ^$\.*+?()[|

        $result = [];
        //      PatternCharacter
        if ($result = $this->matchReg("/^[^^$\\.*+?()[\]{}|]/")) {
            return $this->createCharacter($result);
        }
        //      ExtendedPatternCharacter
        if (!$this->hasUnicodeFlag && $result = $this->matchReg("/^[^^$\\.*+?()[\]{}|]/")) {
            return $this->createCharacter($result);
        }
        //      .
        if ($this->match('.')) {
            return $this->createDot();
        }
        //      \ AtomEscape
        if ($this->match("\\")) {
            $result = $this->parseAtomEscape();
            if (!$result) {
                if (!$this->hasUnicodeFlag && $this->lookahead() === 'c') {
                    // B.1.4 ExtendedAtom
                    // \[lookahead = c]
                    return $this->createValue(
                        self::KIND_SYMBOL,
                        92,
                        $this->pos - 1,
                        $this->pos
                    );
                }
                $this->makeThrow('atomEscape', $this->pos - 1, $this->pos);
            }
        }

        if ($result = $this->parseCharacterClass()) {

        }


    }



    private function parseDecimalEscape()
    {
        // DecimalEscape ::
        //      DecimalIntegerLiteral [lookahead ∉ DecimalDigit]
        //      CharacterClassEscape :: one of d D s S w W
        if ($result = $this->matchReg("/^(?!0)\d+/")) {
            $refIdx = intval($result[0], 10);
            // If the number is smaller than the normal-groups found so
            // far, then it is a reference...
            if ($refIdx <= $this->closedCaptureCounter) {
                return $this->createReference($result[0]);
            }
            // ... otherwise it needs to be interpreted as a octal (if the
            // number is in an octal format). If it is NOT octal format,
            // then the slash is ignored and the number is matched later
            // as normal characters.

            // Recall the negative decision to decide if the input must be parsed
            // a second time with the total normal-groups.
            $this->backrefDenied[] = $refIdx;
            // Reset the position again, as maybe only parts of the previous
            // matched numbers are actual octal numbers. E.g. in '019' only
            // the '01' should be matched.
            $this->incr(-strlen($result[0]));

            if ($result = $this->matchReg("/^[0-7]{1,3}/")) {
                return $this->createEscaped(self::KIND_OCTAL, intval($result[0], 8), $result[0], 1);
            }
            // If we end up here, we have a case like /\91/. Then the
            // first slash is to be ignored and the 9 & 1 to be treated
            // like ordinary characters. Create a character for the
            // first number only here - other number-characters
            // (if available) will be matched later.
            $result = $this->createCharacter($this->matchReg("/^[89]/"));
            return $this->updateRawStart($result, $result['range'][0] - 1);
        }
        // Only allow octal numbers in the following. All matched numbers start
        // with a zero (if the do not, the previous if-branch is executed).
        // If the number is not octal format and starts with zero (e.g. `091`)
        // then only the zeros `0` is treated here and the `91` are ordinary
        // characters.
        // Example:
        //   /\091/.exec('\091')[0].length === 3

        if ($result = $this->matchReg("/^[0-7]{1,3}/")) {
            if (preg_match("/^0{1,3}$/", $result[0], $match)) {
                // If they are all zeros, then only take the first one.
                return $this->createEscaped('null', 0x0000, '0', strlen($result[0]) + 1);
            }
            return $this->createEscaped(self::KIND_OCTAL, intval($result[0], 8), $result[0], 1);
        }

        if ($result = $this->matchReg("/^[dDsSwW]/")) {
            return $this->createCharacterClassEscape($result[0]);
        }

        return false;
    }

    private function parseNamedReference()
    {
        if ($this->namedGroups && $this->matchReg("/^k<(?=.*?>)/")) {
            $name = $this->parseIdentifier();
            $this->skip('>');
            return $this->createNamedReference($name);
        }
    }


    private function parseRegExpUnicodeEscapeSequence()
    {

        if ($res = $this->matchReg("/^u([0-9a-fA-F]{4})/")) {
            // UnicodeEscapeSequence
            return $this->parseUnicodeSurrogatePairEscape(
                $this->createEscaped('unicodeEscape', intval($res[1], 16), $res[1], 2)
            );
        }

        if ($this->hasUnicodeFlag && ($res = $this->matchReg("/^u\{([0-9a-fA-F]+)\}/"))) {
            // RegExpUnicodeEscapeSequence (ES6 Unicode code point escape)
            return $this->createEscaped('unicodeCodePointEscape', intval($res[1], 16), res[1], 4);
        }

        return null;
    }

    private function parseCharacterEscape()
    {
        // CharacterEscape ::
        //      ControlEscape
        //      c ControlLetter
        //      HexEscapeSequence
        //      UnicodeEscapeSequence
        //      IdentityEscape

        $from = $this->pos;
        if ($res = $this->matchReg("/^[fnrtv]/")) {
            // ControlEscape
            $codePointMap = [
                't' => 0x009,
                'n' => 0x00A,
                'v' => 0x00B,
                'f' => 0x00C,
                'r' => 0x00D,
            ];
            $codePoint = $codePointMap[$res[0]] ?? 0;
            return $this->createEscaped('singleEscape', $codePoint, "\\" + $res[0]);
        }
        if ($res = $this->matchReg("/^c([a-zA-Z])/")) {
            // c ControlLetter
            return $this->createEscaped('controlLetter', $this->charCodeAt($res[1], 0) % 32, $res[1], 2);
        }

        if ($res = $this->matchReg("/^x([0-9a-fA-F]{2})/")) {
            // HexEscapeSequence
            return $this->createEscaped('hexadecimalEscape', intval($res[1], 16), $res[1], 2);
        }

        if ($res = $this->parseRegExpUnicodeEscapeSequence()) {
            if (!$res || $res['codePoint'] > 0x10FFFF) {
                $this->makeThrow('Invalid escape sequence', $from, $this->pos);
            }
            return $res;
        }
        if ($this->unicodePropertyEscape && $this->hasUnicodeFlag && ($res = $this->matchReg("/^([pP])\{([^\}]+)\}/"))) {
            // https://github.com/jviereck/regjsparser/issues/77
            return $this->createUnicodePropertyEscape($res);
        }
        // IdentityEscape
        return $this->parseIdentityEscape();
    }


    private function createUnicodePropertyEscape($res)
    {
        $result = $this->createTypeAndRange(
            self::TYPE_UNICODE_PROPERTY_ESCAPE,
            $res['range'][0] - 1,
            $res['range'][1]
        );
        $result['negative'] = $res[1] === 'P';
        $result['value'] = $res[2];
        $result['raw'] = $res[0];

        return $result;
    }

    private function parseIdentifierAtom($check) {
        $ch = $this->lookahead();
        $from = $this->pos;


    }


    private function parseAtomEscape($insideCharacterClass = false)
    {
        // AtomEscape ::
        //      DecimalEscape
        //      CharacterEscape
        //      CharacterClassEscape
        //      k GroupName
        $from = $this->pos;
        $res = $this->parseDecimalEscape() || $this->parseNamedReference();
        if ($res) {
            return $res;
        }

    }




    private function createDot()
    {
        $result = $this->createTypeAndRange(self::TYPE_DOT, $this->pos - 1, $this->pos);
        return $this->addendRaw($result);
    }

    private function charCodeAt($str, $index)
    {
        $char = mb_substr($str, $index, 1, 'UTF-8');
        if (mb_check_encoding($char, 'UTF-8')) {
            $ret = mb_convert_encoding($char, 'UTF-32BE', 'UTF-8');
            return hexdec(bin2hex($ret));
        }
        return null;
    }


    private function createCharacter($matches)
    {
        $char = $matches[0];
        $first = $this->charCodeAt($char, 0);
        if ($this->hasUnicodeFlag) {
            /*
             	var second;
		if (_char.length === 1 && first >= 0xD800 && first <= 0xDBFF) {
		  second = lookahead().charCodeAt(0);
		  if (second >= 0xDC00 && second <= 0xDFFF) {
			// Unicode surrogate pair
			pos++;
			return createValue(
			  'symbol',
			  (first - 0xD800) * 0x400 + second - 0xDC00 + 0x10000,
			  pos - 2, pos);
		  }
		}
             */
        }
        return $this->createValue(self::KIND_SYMBOL, $first, $this->pos - 1, $this->pos);
    }



    private function createValue($kind, $codePoint, $from, $to)
    {
        $result = $this->createTypeAndRange(self::TYPE_ANCHOR, $from, $to);
        $result['kind'] = $kind;
        $result['codePoint'] = $codePoint;
        return $this->addendRaw($result);
    }

    private function parseTerm()
    {
        if ($this->pos >= strlen($this->source) || $this->current('|') || $this->current(')')) {
            return null;
        }

        if ($anchor = $this->parseAnchor()) {
            return $anchor;
        }

        $form = $this->pos;
        if (!$atom = $this->parseAtomAndExtendedAtom()) {
            $this->makeThrow('Expected atom', $form, $this->pos);
        }


        /*
      var atom = parseAtomAndExtendedAtom();
      if (!atom) {
        bail('Expected atom');
      }
      var quantifier = parseQuantifier() || false;
      if (quantifier) {
        quantifier.body = flattenBody(atom);
        // The quantifier contains the atom. Therefore, the beginning of the
        // quantifier range is given by the beginning of the atom.
        updateRawStart(quantifier, atom.range[0]);
        return quantifier;
      }
      return atom;
         */


    }


    private function createTypeAndRange($type, $from, $to)
    {
        $range = [$from, $to];
        return compact('type', 'range');
    }

    private function addendRaw(array $node): array
    {
        $range = $node['range'];
        $node['raw'] = substr($this->source, $range[0], $range[1]);
        return $node;
    }


}