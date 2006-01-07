
#
#  This grammar was closely derived from
#    ECMA-262, ECMAScript Language Specification, 3rd Edition
#    Appendix A.
#
#  Changes to specification grammar.
#   * RegularExpressionLiteral has been added to Literal.
#     Since we dont have a separate tokenizer.
#   * <SourceCharacter> removals.
#     Several "SourceCharacter but not ..." have been expressed as
#     complemented character sets.  <-[...]>  They are all tagged with
#     #SourceCharacter but not ...
#     But <SourceCharacter> isnt called for all characters anyway,
#     so no great loss.
#
#  Other notes:
#   * <ws> is redefined to be used, via :words, by A.3, A.4, and A.5.
#     Note it is always an optional match.
#   * Parts of the grammar, intended for a tokenizer, are not actually used.
#   * Some rules, eg StringNumericLiteral, are not used in parsing JavaScript,
#     but rather in implementing it (eg, by ToNumber).
#   * Character sets are sometimes collapsed.
#     Eg, <'0'> | <'1'> ... <'9'> changed to <[0..9]>
#   * "Foo but not x" is expressed as
#     <!before x> <Foo>
#   * "lookahead is not an element of { x }" is expressed as
#     <!before x>
#     Such usage is tagged with #lookahead
#     If rules taking arguments had any hope of working soon,
#     I would have said <!lookahead x>.
#   * "[no LineTerminator here]" is expressed as
#     <no_LineTerminator_here>.
#
#  Resources:
#   http://www.ecma-international.org/publications/standards/Ecma-262.htm
#   http://bclary.com/2004/11/07/ecma-262
#
#  The SPEC grammar SHOULD NOT BE CHANGED to accommodate the characteristics
#  of particular regex engines.  Its structure follows the spec.  If for
#  instance, your engine cannot handle left recursive rules, you should
#  create a new grammar, inherit from this one, and redefine just those
#  rules.  See JavaScript::ECMAScript3::Grammar.
#
#  Bugs:
#   * This grammar has never yet been run, so there no doubt are some.
#
#  Possible Bugs:
#   * We are not using some of the tokenizer rules.
#     So it isnt entirely clear we are doing an fully correct parse.
#
#  Todo:
#   * Create a second grammar which actually runs, is efficient,
#     and has captures for easy use of the resulting matches.
#   * Perhaps define a macro gram ($name) { "rule $name :words" },
#     rather than having the clutter of lots of explicit :words modifiers.
#
#  Thanks to Bob Clary, who created the HTML-ified ecma-262, without which
#  this grammar would likely not have been written. - Mitchell N Charity
#


grammar JavaScript::ECMAScript3::Grammar::Spec;

rule ws          {<?ws_input>*}
rule ws_required {<?ws_input>+}
rule ws_input {
      <WhiteSpace>
    | <LineTerminator>
    | <Comment>
}  
rule no_LineTerminator_here {
      [ <ws> & <-<LineTerminator>>*? ]
}


# 7.2 White Space

rule TAB  { \x0009 }
rule VT   { \x000B }
rule FF   { \x000C }
rule SP   { \x0020 }
rule NBSP { \x00A0 }
rule USP  { <<Zs>-<TAB>-<VT>-<FF>-<SP>-<NBSP>> }

# 7.3 Line Terminators

rule LF   { \x000A }
rule CR   { \x000D }
rule LS   { \x2028 }
rule PS   { \x2029 }


# A.1 Lexical Grammar

rule SourceCharacter {  # see clause 6
      . #dot
}
rule InputElementDiv {  # see clause 7
      <WhiteSpace>
    | <LineTerminator>
    | <Comment>
    | <Token> <DivPunctuator>
}
rule InputElementRegExp {  # see clause 7
      <WhiteSpace>
    | <LineTerminator>
    | <Comment>
    | <Token>
    | <RegularExpressionLiteral>
}
rule WhiteSpace {  # see 7.2
      <TAB>
    | <VT>
    | <FF>
    | <SP>
    | <NBSP>
    | <USP>
}
rule LineTerminator {  # see 7.3
      <LF>
    | <CR>
    | <LS>
    | <PS>
}
rule Comment {  # see 7.4
      <MultiLineComment>
    | <SingleLineComment>
}
rule MultiLineComment {  # see 7.4
      <'/*'> <MultiLineCommentChars>? <'*/'>
}
rule MultiLineCommentChars {  # see 7.4
      <MultiLineNotAsteriskChar> <MultiLineCommentChars>?
    | <'*'> <PostAsteriskCommentChars>?
}
rule PostAsteriskCommentChars {  # see 7.4
      <MultiLineNotForwardSlashOrAsteriskChar> <MultiLineCommentChars>?
    | <'*'> <PostAsteriskCommentChars>?
}
rule MultiLineNotAsteriskChar {  # see 7.4
      <-[*]> #SourceCharacter but not ...
}
rule MultiLineNotForwardSlashOrAsteriskChar {  # see 7.4
      <-[/*]> #SourceCharacter but not ...
}
rule SingleLineComment {  # see 7.4
      <'//'> <SingleLineCommentChars>?
}
rule SingleLineCommentChars {  # see 7.4
      <SingleLineCommentChar> <SingleLineCommentChars>?
}
rule SingleLineCommentChar {  # see 7.4
      <!before <LineTerminator>> <SourceCharacter>
}
rule Token {  # see 7.5
      <ReservedWord>
    | <Identifier>
    | <Punctuator>
    | <NumericLiteral>
    | <StringLiteral>
}
rule ReservedWord {  # see 7.5.1
      <Keyword>
    | <FutureReservedWord>
    | <NullLiteral>
    | <BooleanLiteral>
}
rule Keyword {  # see 7.5.2
      <'break'>
    | <'else'>
    | <'new'>
    | <'var'>
    | <'case'>
    | <'finally'>
    | <'return'>
    | <'void'>
    | <'catch'>
    | <'for'>
    | <'switch'>
    | <'while'>
    | <'continue'>
    | <'function'>
    | <'this'>
    | <'with'>
    | <'default'>
    | <'if'>
    | <'throw'>
    | <'delete'>
    | <'in'>
    | <'try'>
    | <'do'>
    | <'instanceof'>
    | <'typeof'>
}
rule FutureReservedWord {  # see 7.5.3
      <'abstract'>
    | <'enum'>
    | <'int'>
    | <'short'>
    | <'boolean'>
    | <'export'>
    | <'interface'>
    | <'static'>
    | <'byte'>
    | <'extends'>
    | <'long'>
    | <'super'>
    | <'char'>
    | <'final'>
    | <'native'>
    | <'synchronized'>
    | <'class'>
    | <'float'>
    | <'package'>
    | <'throws'>
    | <'const'>
    | <'goto'>
    | <'private'>
    | <'transient'>
    | <'debugger'>
    | <'implements'>
    | <'protected'>
    | <'volatile'>
    | <'double'>
    | <'import'>
    | <'public'>
}
rule Identifier {  # see 7.6
      <!before <ReservedWord>> <IdentifierName>
}
rule IdentifierName {  # see 7.6
      <IdentifierStart>
    | <IdentifierName> <IdentifierPart>
}
rule IdentifierStart {  # see 7.6
      <UnicodeLetter>
    | <'$'>
    | <'_'>
    | <'\\'> <UnicodeEscapeSequence>
}
rule IdentifierPart {  # see 7.6
      <IdentifierStart>
    | <UnicodeCombiningMark>
    | <UnicodeDigit>
    | <UnicodeConnectorPunctuation>
    | <'\\'> <UnicodeEscapeSequence>
}
rule UnicodeLetter {  # see 7.6
      # any character in the Unicode categories "Uppercase letter (Lu)",
      # "Lowercase letter (Ll)", "Titlecase letter (Lt)",
      # "Modifier letter (Lm)", "Other letter (Lo)", or "Letter number (Nl)".
      <Lu> | <Ll> | <Lt> | <Lm> | <Lo> | <Nl>
}
rule UnicodeCombiningMark {  # see 7.6
      # any character in the Unicode categories "Non-spacing mark (Mn)"
      # or "Combining spacing mark (Mc)"
      <Mn> | <Mc>
}
rule UnicodeDigit {  # see 7.6
      # any character in the Unicode category "Decimal number (Nd)"
      <Nd>
}
rule UnicodeConnectorPunctuation {  # see 7.6
      # any character in the Unicode category "Connector punctuation (Pc)"
      <Pc>
}
rule HexDigit {  # see 7.6
      <[0..9a..fA..F]>
}
rule Punctuator {  # see 7.7
      <'{'>
    | <'}'>
    | <'('>
    | <')'>
    | <'['>
    | <']'>
    | <'.'>
    | <';'>
    | <','>
    | <'<'>
    | <'>'>
    | <'<='>
    | <'>='>
    | <'=='>
    | <'!='>
    | <'==='>
    | <'!=='>
    | <'+'>
    | <'-'>
    | <'*'>
    | <'%'>
    | <'++'>
    | <'--'>
    | <'<<'>
    | <'>>'>
    | <'>>>'>
    | <'&'>
    | <'|'>
    | <'^'>
    | <'!'>
    | <'~'>
    | <'&&'>
    | <'||'>
    | <'?'>
    | <':'>
    | <'='>
    | <'+='>
    | <'-='>
    | <'*='>
    | <'%='>
    | <'<<='>
    | <'>>='>
    | <'>>>='>
    | <'&='>
    | <'|='>
    | <'^='>
}
rule DivPunctuator {  # see 7.7
      <'/'>
    | <'/='>
}
rule Literal {  # see 7.8
      <NullLiteral>
    | <BooleanLiteral>
    | <NumericLiteral>
    | <StringLiteral>
    | <RegularExpressionLiteral> # ADDED - this is not in the spec.
}
rule NullLiteral {  # see 7.8.1
      <'null'>
}
rule BooleanLiteral {  # see 7.8.2
      <'true'>
    | <'false'>
}
rule NumericLiteral {  # see 7.8.3
      <DecimalLiteral>
    | <HexIntegerLiteral>
}
rule DecimalLiteral {  # see 7.8.3
      <DecimalIntegerLiteral> <'.'> <DecimalDigits>? <ExponentPart>?
    | <'.'> <DecimalDigits> <ExponentPart>?
    | <DecimalIntegerLiteral> <ExponentPart>?
}
rule DecimalIntegerLiteral {  # see 7.8.3
      <'0'>
    | <NonZeroDigit> <DecimalDigits>?
}
rule DecimalDigits {  # see 7.8.3
      <DecimalDigit>
    | <DecimalDigits> <DecimalDigit>
}
rule DecimalDigit {  # see 7.8.3
      <[0..9]>
}
rule ExponentIndicator {  # see 7.8.3
      <'e'>
    | <'E'>
}
rule SignedInteger {  # see 7.8.3
      <DecimalDigits>
    | <'+'> <DecimalDigits>
    | <'-'> <DecimalDigits>
}
rule HexIntegerLiteral {  # see 7.8.3
      <'0x'> <HexDigit>
    | <'0X'> <HexDigit>
    | <HexIntegerLiteral> <HexDigit>
}
rule StringLiteral {  # see 7.8.4
      <'"'> <DoubleStringCharacters>? <'"'>
    | <'\''> <SingleStringCharacters>? <'\''>
}
rule DoubleStringCharacters {  # see 7.8.4
      <DoubleStringCharacter> <DoubleStringCharacters>?
}
rule SingleStringCharacters {  # see 7.8.4
      <SingleStringCharacter> <SingleStringCharacters>?
}
rule DoubleStringCharacter {  # see 7.8.4
      <!before <LineTerminator>> <-[\"\\]> #SourceCharacter but not ...
    | <'\\'> <EscapeSequence>
}
rule SingleStringCharacter {  # see 7.8.4
      <!before <LineTerminator>> <-[\'\\]> #SourceCharacter but not ...
    | <'\\'> <EscapeSequence>
}
rule EscapeSequence {  # see 7.8.4
      <CharacterEscapeSequence>
    | <'0'> <!before <DecimalDigit>> #lookahead
    | <HexEscapeSequence>
    | <UnicodeEscapeSequence>
}
rule CharacterEscapeSequence {  # see 7.8.4
      <SingleEscapeCharacter>
    | <NonEscapeCharacter>
}
rule SingleEscapeCharacter {  # see 7.8.4
      <'\''>
    | <'"'>
    | <'\\'>
    | <'b'>
    | <'f'>
    | <'n'>
    | <'r'>
    | <'t'>
    | <'v'>
}
rule EscapeCharacter {  # see 7.8.4
      <SingleEscapeCharacter>
    | <DecimalDigit>
    | <'x'>
    | <'u'>
}
rule HexEscapeSequence {  # see 7.8.4
      <'x'> <HexDigit> <HexDigit>
}
rule UnicodeEscapeSequence {  # see 7.8.4
      <'u'> <HexDigit> <HexDigit> <HexDigit> <HexDigit>
}
rule RegularExpressionLiteral {  # see 7.8.5
      <'/'> <RegularExpressionBody> <'/'> <RegularExpressionFlags>
}
rule RegularExpressionBody {  # see 7.8.5
      <RegularExpressionFirstChar> <RegularExpressionChars>
}
rule RegularExpressionChars {  # see 7.8.5
      <null>
    | <RegularExpressionChars> <RegularExpressionChar>
}
rule RegularExpressionFirstChar {  # see 7.8.5
      <!before <[*\\\/]>> <NonTerminator>
    | <BackslashSequence>
}
rule RegularExpressionChar {  # see 7.8.5
      <!before <[\\\/]>> <NonTerminator>
    | <BackslashSequence>
}
rule BackslashSequence {  # see 7.8.5
      <'\\'> <NonTerminator>
}
rule NonTerminator {  # see 7.8.5
      <!before <LineTerminator>> <SourceCharacter>
}
rule RegularExpressionFlags {  # see 7.8.5
      <null>
    | <RegularExpressionFlags> <IdentifierPart>
}

# A.2 Number Conversions

rule StringNumericLiteral {  # see 9.3.1
      <StrWhiteSpace>?
    | <StrWhiteSpace>? <StrNumericLiteral> <StrWhiteSpace>?
}
rule StrWhiteSpace {  # see 9.3.1
      <StrWhiteSpaceChar> <StrWhiteSpace>?
}
rule StrWhiteSpaceChar {  # see 9.3.1
      <TAB>
    | <SP>
    | <NBSP>
    | <FF>
    | <VT>
    | <CR>
    | <LF>
    | <LS>
    | <PS>
    | <USP>
}
rule StrNumericLiteral {  # see 9.3.1
      <StrDecimalLiteral>
    | <HexIntegerLiteral>
}
rule StrDecimalLiteral {  # see 9.3.1
      <StrUnsignedDecimalLiteral>
    | <'+'> <StrUnsignedDecimalLiteral>
    | <'-'> <StrUnsignedDecimalLiteral>
}
rule StrUnsignedDecimalLiteral {  # see 9.3.1
      <'Infinity'>
    | <DecimalDigits> <'.'> <DecimalDigits>? <ExponentPart>?
    | <'.'> <DecimalDigits> <ExponentPart>?
    | <DecimalDigits> <ExponentPart>?
}
rule DecimalDigits {  # see 9.3.1
      <DecimalDigit>
    | <DecimalDigits> <DecimalDigit>
}
rule DecimalDigit {  # see 9.3.1
      <[0..9]>
}
rule ExponentPart {  # see 9.3.1
      <ExponentIndicator> <SignedInteger>
}
rule ExponentIndicator {  # see 9.3.1
      <'e'>
    | <'E'>
}
rule SignedInteger {  # see 9.3.1
      <DecimalDigits>
    | <'+'> <DecimalDigits>
    | <'-'> <DecimalDigits>
}
rule HexIntegerLiteral {  # see 9.3.1
      <'0x'> <HexDigit>
    | <'0X'> <HexDigit>
    | <HexIntegerLiteral> <HexDigit>
}
rule HexDigit {  # see 9.3.1
      <[0..9a..fA..F]>
}

# A.3 Expressions

rule PrimaryExpression :words {  # see 11.1
      <'this'>
    | <Identifier>
    | <Literal>
    | <ArrayLiteral>
    | <ObjectLiteral>
    | <'('> <Expression> <')'>
}
rule ArrayLiteral :words {  # see 11.1.4
      <'['> <Elision>? <']'>
    | <'['> <ElementList> <']'>
    | <'['> <ElementList> <','> <Elision>? <']'>
}
rule ElementList :words {  # see 11.1.4
      <Elision>? <AssignmentExpression>
    | <ElementList> <','> <Elision>? <AssignmentExpression>
}
rule Elision :words {  # see 11.1.4
      <','>
    | <Elision> <','>
}
rule ObjectLiteral :words {  # see 11.1.5
      <'{'> <'}'>
    | <'{'> <PropertyNameAndValueList> <'}'>
}
rule PropertyNameAndValueList :words {  # see 11.1.5
      <PropertyName> <':'> <AssignmentExpression>
    | <PropertyNameAndValueList> <','> <PropertyName> <':'> <AssignmentExpression>
}
rule PropertyName :words {  # see 11.1.5
      <Identifier>
    | <StringLiteral>
    | <NumericLiteral>
}
rule MemberExpression :words {  # see 11.2
      <PrimaryExpression>
    | <FunctionExpression>
    | <MemberExpression> <'['> <Expression> <']'>
    | <MemberExpression> <'.'> <Identifier>
    | <'new'> <?ws_required> <MemberExpression> <Arguments>
}
rule NewExpression :words  {  # see 11.2
      <MemberExpression>
    | <'new'> <?ws_required> <NewExpression>
}
rule CallExpression :words {  # see 11.2
      <MemberExpression> <Arguments>
    | <CallExpression> <Arguments>
    | <CallExpression> <'['> <Expression> <']'>
    | <CallExpression> <'.'> <Identifier>
}
rule Arguments :words {  # see 11.2
      <'('> <')'>
    | <'('> <ArgumentList> <')'>
}
rule ArgumentList :words {  # see 11.2
      <AssignmentExpression>
    | <ArgumentList> <','> <AssignmentExpression>
}
rule LeftHandSideExpression :words {  # see 11.2
      <NewExpression>
    | <CallExpression>
}
rule PostfixExpression :words {  # see 11.3
      <LeftHandSideExpression>
    | <LeftHandSideExpression><no_LineTerminator_here><'++'>
    | <LeftHandSideExpression><no_LineTerminator_here><'--'>
}
rule UnaryExpression :words {  # see 11.4
      <PostfixExpression>
    | <'delete'> <UnaryExpression>
    | <'void'> <UnaryExpression>
    | <'typeof'> <UnaryExpression>
    | <'++'> <UnaryExpression>
    | <'--'> <UnaryExpression>
    | <'+'> <UnaryExpression>
    | <'-'> <UnaryExpression>
    | <'~'> <UnaryExpression>
    | <'!'> <UnaryExpression>
}
rule MultiplicativeExpression :words {  # see 11.5
      <UnaryExpression>
    | <MultiplicativeExpression> <'*'> <UnaryExpression>
    | <MultiplicativeExpression> <'/'> <UnaryExpression>
    | <MultiplicativeExpression> <'%'> <UnaryExpression>
}
rule AdditiveExpression :words {  # see 11.6
      <MultiplicativeExpression>
    | <AdditiveExpression> <'+'> <MultiplicativeExpression>
    | <AdditiveExpression> <'-'> <MultiplicativeExpression>
}
rule ShiftExpression :words {  # see 11.7
      <AdditiveExpression>
    | <ShiftExpression> <'<<'> <AdditiveExpression>
    | <ShiftExpression> <'>>'> <AdditiveExpression>
    | <ShiftExpression> <'>>>'> <AdditiveExpression>
}
rule RelationalExpression :words {  # see 11.8
      <ShiftExpression>
    | <RelationalExpression> <'<'> <ShiftExpression>
    | <RelationalExpression> <'>'> <ShiftExpression>
    | <RelationalExpression> <'<='> <ShiftExpression>
    | <RelationalExpression> <'>='> <ShiftExpression>
    | <RelationalExpression> <'instanceof'> <ShiftExpression>
    | <RelationalExpression> <'in'> <ShiftExpression>
}
rule RelationalExpressionNoIn :words {  # see 11.8
      <ShiftExpression>
    | <RelationalExpressionNoIn> <'<'> <ShiftExpression>
    | <RelationalExpressionNoIn> <'>'> <ShiftExpression>
    | <RelationalExpressionNoIn> <'<='> <ShiftExpression>
    | <RelationalExpressionNoIn> <'>='> <ShiftExpression>
    | <RelationalExpressionNoIn> <'instanceof'> <ShiftExpression>
}
rule EqualityExpression :words {  # see 11.9
      <RelationalExpression>
    | <EqualityExpression> <'=='> <RelationalExpression>
    | <EqualityExpression> <'!='> <RelationalExpression>
    | <EqualityExpression> <'==='> <RelationalExpression>
    | <EqualityExpression> <'!=='> <RelationalExpression>
}
rule EqualityExpressionNoIn :words {  # see 11.9
      <RelationalExpressionNoIn>
    | <EqualityExpressionNoIn> <'=='> <RelationalExpressionNoIn>
    | <EqualityExpressionNoIn> <'!='> <RelationalExpressionNoIn>
    | <EqualityExpressionNoIn> <'==='> <RelationalExpressionNoIn>
    | <EqualityExpressionNoIn> <'!=='> <RelationalExpressionNoIn>
}
rule BitwiseANDExpression :words {  # see 11.10
      <EqualityExpression>
    | <BitwiseANDExpression> <'&'> <EqualityExpression>
}
rule BitwiseANDExpressionNoIn :words {  # see 11.10
      <EqualityExpressionNoIn>
    | <BitwiseANDExpressionNoIn> <'&'> <EqualityExpressionNoIn>
}
rule BitwiseXORExpression :words {  # see 11.10
      <BitwiseANDExpression>
    | <BitwiseXORExpression> <'^'> <BitwiseANDExpression>
}
rule BitwiseXORExpressionNoIn :words {  # see 11.10
      <BitwiseANDExpressionNoIn>
    | <BitwiseXORExpressionNoIn> <'^'> <BitwiseANDExpressionNoIn>
}
rule BitwiseORExpression :words {  # see 11.10
      <BitwiseXORExpression>
    | <BitwiseORExpression> <'|'> <BitwiseXORExpression>
}
rule BitwiseORExpressionNoIn :words {  # see 11.10
      <BitwiseXORExpressionNoIn>
    | <BitwiseORExpressionNoIn> <'|'> <BitwiseXORExpressionNoIn>
}
rule LogicalANDExpression :words {  # see 11.11
      <BitwiseORExpression>
    | <LogicalANDExpression> <'&&'> <BitwiseORExpression>
}
rule LogicalANDExpressionNoIn :words {  # see 11.11
      <BitwiseORExpressionNoIn>
    | <LogicalANDExpressionNoIn> <'&&'> <BitwiseORExpressionNoIn>
}
rule LogicalORExpression :words {  # see 11.11
      <LogicalANDExpression>
    | <LogicalORExpression> <'||'> <LogicalANDExpression>
}
rule LogicalORExpressionNoIn :words {  # see 11.11
      <LogicalANDExpressionNoIn>
    | <LogicalORExpressionNoIn> <'||'> <LogicalANDExpressionNoIn>
}
rule ConditionalExpression :words {  # see 11.12
      <LogicalORExpression>
    | <LogicalORExpression> <'?'> <AssignmentExpression> <':'> <AssignmentExpression>
}
rule ConditionalExpressionNoIn :words {  # see 11.12
      <LogicalORExpressionNoIn>
    | <LogicalORExpressionNoIn> <'?'> <AssignmentExpressionNoIn> <':'> <AssignmentExpressionNoIn>
}
rule AssignmentExpression :words {  # see 11.13
      <ConditionalExpression>
    | <LeftHandSideExpression> <AssignmentOperator> <AssignmentExpression>
}
rule AssignmentExpressionNoIn :words {  # see 11.13
      <ConditionalExpressionNoIn>
    | <LeftHandSideExpression> <AssignmentOperator> <AssignmentExpressionNoIn>
}
rule AssignmentOperator :words {  # see 11.13
      <'='>
    | <'*='>
    | <'/='>
    | <'%='>
    | <'+='>
    | <'-='>
    | <'<<='>
    | <'>>='>
    | <'>>>='>
    | <'&='>
    | <'^='>
    | <'|='>
}
rule Expression :words {  # see 11.14
      <AssignmentExpression>
    | <Expression> <','> <AssignmentExpression>
}
rule ExpressionNoIn :words {  # see 11.14
      <AssignmentExpressionNoIn>
    | <ExpressionNoIn> <','> <AssignmentExpressionNoIn>
}

# A.4 Statements

rule Statement :words {  # see clause 12
      <Block>
    | <VariableStatement>
    | <EmptyStatement>
    | <ExpressionStatement>
    | <IfStatement>
    | <IterationStatement>
    | <ContinueStatement>
    | <BreakStatement>
    | <ReturnStatement>
    | <WithStatement>
    | <LabelledStatement>
    | <SwitchStatement>
    | <ThrowStatement>
    | <TryStatement>
}
rule Block :words {  # see 12.1
      <'{'> <StatementList>? <'}'>
}
rule StatementList :words {  # see 12.1
      <Statement>
    | <StatementList> <Statement>
}
rule VariableStatement :words {  # see 12.2
      <'var'> <?ws_required> <VariableDeclarationList> <';'>
}
rule VariableDeclarationList :words {  # see 12.2
      <VariableDeclaration>
    | <VariableDeclarationList> <','> <VariableDeclaration>
}
rule VariableDeclarationListNoIn :words {  # see 12.2
      <VariableDeclarationNoIn>
    | <VariableDeclarationListNoIn> <','> <VariableDeclarationNoIn>
}
rule VariableDeclaration :words {  # see 12.2
      <Identifier> <Initialiser>?
}
rule VariableDeclarationNoIn :words {  # see 12.2
      <Identifier> <InitialiserNoIn>?
}
rule Initialiser :words {  # see 12.2
      <'='> <AssignmentExpression>
}
rule InitialiserNoIn :words {  # see 12.2
      <'='> <AssignmentExpressionNoIn>
}
rule EmptyStatement :words {  # see 12.3
      <';'>
}
rule ExpressionStatement :words {  # see 12.4
      <!before <'function'>><Expression> <';'> #lookahead
}
rule IfStatement :words {  # see 12.5
      <'if'> <'('> <Expression> <')'> <Statement> <'else'> <Statement>
    | <'if'> <'('> <Expression> <')'> <Statement>
}
rule IterationStatement :words {  # see 12.6
      <'do'> <Statement> <'while'> <'('> <Expression> <');'>
    | <'while'> <'('> <Expression> <')'> <Statement>
    | <'for'> <'('> <ExpressionNoIn>? <';'> <Expression>? <';'> <Expression>? <')'> <Statement>
    | <'for'> <'('> <'var'> <VariableDeclarationListNoIn> <';'> <Expression>? <';'> <Expression>? <')'> <Statement>
    | <'for'> <'('> <LeftHandSideExpression> <'in'> <Expression> <')'> <Statement>
    | <'for'> <'('> <'var'> <VariableDeclarationNoIn> <'in'> <Expression> <')'> <Statement>
}
rule ContinueStatement :words {  # see 12.7
      <'continue'><no_LineTerminator_here><Identifier>? <';'>
}
rule BreakStatement :words {  # see 12.8
      <'break'><no_LineTerminator_here><Identifier>? <';'>
}
rule ReturnStatement :words {  # see 12.9
      <'return'><no_LineTerminator_here><Expression>? <';'>
}
rule WithStatement :words {  # see 12.10
      <'with'> <'('> <Expression> <')'> <Statement>
}
rule SwitchStatement :words {  # see 12.11
      <'switch'> <'('> <Expression> <')'> <CaseBlock>
}
rule CaseBlock :words {  # see 12.11
      <'{'> <CaseClauses>? <'}'>
    | <'{'> <CaseClauses>? <DefaultClause> <CaseClauses>? <'}'>
}
rule CaseClauses :words {  # see 12.11
      <CaseClause>
    | <CaseClauses> <CaseClause>
}
rule CaseClause :words {  # see 12.11
      <'case'> <Expression> <':'> <StatementList>?
}
rule DefaultClause :words {  # see 12.11
      <'default'> <':'> <StatementList>?
}
rule LabelledStatement :words {  # see 12.12
      <Identifier> <':'> <Statement>
}
rule ThrowStatement :words {  # see 12.13
      <'throw'><no_LineTerminator_here><Expression> <';'>
}
rule TryStatement :words {  # see 12.14
      <'try'> <Block> <Catch>
    | <'try'> <Block> <Finally>
    | <'try'> <Block> <Catch> <Finally>
}
rule Catch :words {  # see 12.14
      <'catch'> <'('> <Identifier> <')'> <Block>
}
rule Finally :words {  # see 12.14
      <'finally'> <Block>
}

# A.5 Functions and Programs

rule FunctionDeclaration :words {  # see clause 13
      <'function'> <Identifier> <'('> <FormalParameterList>? <')'. <'{'> <FunctionBody> <'}'>
}
rule FunctionExpression :words {  # see clause 13
      <'function'> <Identifier>? <'('> <FormalParameterList>? <')'> <'{'> <FunctionBody> <'}'>
}
rule FormalParameterList :words {  # see clause 13
      <Identifier>
    | <FormalParameterList> <','> <Identifier>
}
rule FunctionBody :words {  # see clause 13
      <SourceElements>
}
rule Program :words {  # see clause 14
      <SourceElements>
}
rule SourceElements :words {  # see clause 14
      <SourceElement>
    | <SourceElements> <SourceElement>
}
rule SourceElement :words {  # see clause 14
      <Statement> <FunctionDeclaration>
}

# A.6 Universal Resource Identifier Character Classes

rule uri {  # see 15.1.3
      <uriCharacters>?
}
rule uriCharacters {  # see 15.1.3
      <uriCharacter> <uriCharacters>?
}
rule uriCharacter {  # see 15.1.3
      <uriReserved>
    | <uriUnescaped>
    | <uriEscaped>
}
rule uriReserved {  # see 15.1.3
      <';'>
    | <'/'>
    | <'?'>
    | <':'>
    | <'@'>
    | <'&'>
    | <'='>
    | <'+'>
    | <'$'>
    | <','>
}
rule uriUnescaped {  # see 15.1.3
      <uriAlpha>
    | <DecimalDigit>
    | <uriMark>
}
rule uriEscaped {  # see 15.1.3
      <'%'> <HexDigit> <HexDigit>
}
rule uriAlpha {  # see 15.1.3
      <[a..zA..Z]>
}
rule uriMark {  # see 15.1.3
      <'-'>
    | <'_'>
    | <'.'>
    | <'!'>
    | <'~'>
    | <'*'>
    | <'\''>
    | <'('>
    | <')'>
}

# A.7 Regular Expressions

rule Pattern {  # see 15.10.1
      <Disjunction>
}
rule Disjunction {  # see 15.10.1
      <Alternative>
    | <Alternative> <'|'> <Disjunction>
}
rule Alternative {  # see 15.10.1
      <null>
    | <Alternative> <Term>
}
rule Term {  # see 15.10.1
      <Assertion>
    | <Atom>
    | <Atom> <Quantifier>
}
rule Assertion {  # see 15.10.1
      <'^'>
    | <'$'>
    | <'\\'> <'b'>
    | <'\\'> <'B'>
}
rule Quantifier {  # see 15.10.1
      <QuantifierPrefix>
    | <QuantifierPrefix> <'?'>
}
rule QuantifierPrefix {  # see 15.10.1
      <'*'>
    | <'+'>
    | <'?'>
    | <'{'> <DecimalDigits> <'}'>
    | <'{'> <DecimalDigits> <',}'>
    | <'{'> <DecimalDigits> <','> <DecimalDigits> <'}'>
}
rule Atom {  # see 15.10.1
      <PatternCharacter>
    | <'.'>
    | <'\\'> <AtomEscape>
    | <CharacterClass>
    | <'('> <Disjunction> <')'>
    | <'(?:'> <Disjunction> <')'>
    | <'(?='> <Disjunction> <')'>
    | <'(?!'> <Disjunction> <')'>
}
rule PatternCharacter {  # see 15.10.1
      <-[^$\\.*+?()[\]{}|]> #SourceCharacter but not ...
}
rule AtomEscape {  # see 15.10.1
      <DecimalEscape>
    | <CharacterEscape>
    | <CharacterClassEscape>
}
rule CharacterEscape {  # see 15.10.1
      <ControlEscape>
    | <'c'> <ControlLetter>
    | <HexEscapeSequence>
    | <UnicodeEscapeSequence>
    | <IdentityEscape>
}
rule ControlEscape {  # see 15.10.1
      <'f'>
    | <'n'>
    | <'r'>
    | <'t'>
    | <'v'>
}
rule ControlLetter {  # see 15.10.1
      <[a..zA..Z]>
}
rule IdentityEscape {  # see 15.10.1
      <!before <IdentifierPart>> <SourceCharacter>
}
rule DecimalEscape {  # see 15.10.1
      <DecimalIntegerLiteral> <!before <DecimalDigit>> #lookahead
}
rule CharacterClass {  # see 15.10.1
      <'['> <!before <'^'>> <ClassRanges> <']'> #lookahead
    | <'[^'> <ClassRanges> <']'>
}
rule ClassRanges {  # see 15.10.1
      <null>
    | <NonemptyClassRanges>
}
rule NonemptyClassRanges {  # see 15.10.1
      <ClassAtom>
    | <ClassAtom> <NonemptyClassRangesNoDash>
    | <ClassAtom> <'-'> <ClassAtom> <ClassRanges>
}
rule NonemptyClassRangesNoDash {  # see 15.10.1
      <ClassAtom>
    | <ClassAtomNoDash> <NonemptyClassRangesNoDash>
    | <ClassAtomNoDash> <'-'> <ClassAtom> <ClassRanges>
}
rule ClassAtom {  # see 15.10.1
      <'-'>
    | <ClassAtomNoDash>
}
rule ClassAtomNoDash {  # see 15.10.1
      <-[\\\]\-]> #SourceCharacter but not ...
    | <'\\'>
    | <ClassEscape>
}
rule ClassEscape {  # see 15.10.1
      <DecimalEscape>
    | <'b'>
    | <CharacterEscape>
    | <CharacterClassEscape>
}



grammar JavaScript::ECMAScript3::Grammar is JavaScript::ECMAScript3::Grammar::Spec;

# Intended to be a working, usable grammar.
# Unimplemented.
