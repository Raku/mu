using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Parser.Tokens;
using Tycho.Compiler;
//using Tycho.Lexer;
using Tycho.Language;

namespace Tycho.Parser {
    /*public class MacroParser : Parser, IParser {
        Dictionary<string, IParser> Parsers;
        Dictionary<AnyObject, AnyObject> Macros;
        MacroBuilder MacroBuilder;

        public static Symbol MacroTermName = Namespaces.Parser.Get ("macro");
        public static Symbol SyntaxTermName = Namespaces.Parser.Get ("syntax");

        public MacroParser (string name, Dictionary<string, IParser> parsers, MacroBuilder macroBuilder) : this (name, parsers, macroBuilder, null) { }

        public MacroParser (string name, Dictionary<string, IParser> parsers, MacroBuilder macroBuilder, MacroParser parentParser) : base (name, parentParser) {
            Parsers = parsers;
            Macros = new Dictionary<AnyObject,AnyObject> ();

            MacroBuilder = macroBuilder;

            if (parentParser != null) {
                foreach (KeyValuePair<AnyObject, AnyObject> macro in parentParser.Macros) {
                    Macros.Add (macro.Key, macro.Value);
                }
            }
        }

        Syntax MakeMacroSyntax (string name, IParser macroParser, IParser tokensParser, bool optionalBody) {
            BracketPattern bodyPattern = new BracketPattern (
                Pattern.Sequence (
                    null,
                    false,
                    new DelimitedPattern (
                        new ProductionPattern (Namespaces.Parser.Get ("body"), macroParser, false),
                        new DualKeywordPattern (";", ","))),
                BracketType.Brace);

            Pattern maybeOptionalBody = optionalBody ? (Pattern) new OptionalPattern (bodyPattern) : bodyPattern;

            return new Syntax (
                Namespaces.Parser.Get (name),
                false,
                new KeywordPattern (name),
                new ProductionPattern (Namespaces.Parser.Get ("syntax"), tokensParser, false),
                maybeOptionalBody);
        }

        public void SetupMacroSyntax (IParser macroParser, IParser tokensParser) {
            if (macroParser != null && tokensParser != null) {
                AddSyntax (MakeMacroSyntax ("macro", this, tokensParser, false));
                AddSyntax (MakeMacroSyntax ("syntax", macroParser, tokensParser, true));
            }

            if (macroParser != null) {
                HighPrecedencePatterns.Add (
                    new Syntax (
                        Namespaces.Parser.Get ("unquote"),
                        false,
                        new KeywordPattern ("#"),
                        new ProductionPattern (Namespaces.Parser.Get ("expression"), macroParser, false)).Pattern);
            }
        }

        public override IParser ExtendParser () {
            return new MacroParser (Name, Parsers, MacroBuilder, this);
        }

        protected override AnyObject ConstructTerm (MatchResult result, List<Token> tokens, int startIndex) {
            AnyObject term = base.ConstructTerm (result, tokens, startIndex);
            AnyObject termName = term.GetProperty (Symbols.ParserTermName);

            if (SyntaxTermName == termName) {
                AddMacro (term, false);
                return ExpressionCompiler.Null (term.SourceLocation);
            } else if (MacroTermName == termName) {
                AddMacro (term, true);
                return ExpressionCompiler.Null (term.SourceLocation);
            } else {
                AnyObject macro;
                if (Macros.TryGetValue (termName, out macro)) {
                    return macro.Invoke (term);
                }
            }

            return term;
        }

        public void AddMacro (AnyObject macroName, IEnumerable<AnyObject> productionNames, AnyObject sourceLocationTerm, AnyObject body, bool isQuote) {
            AnyObject macro = MacroBuilder.BuildMacro (productionNames, sourceLocationTerm.SourceLocation, body, isQuote);
            Macros.Add (macroName, macro);
        }

        void AddMacro (AnyObject term, bool isQuote) {
            List<Token> syntaxTokens = term.GetProperty (Namespaces.Parser.Get ("syntax")).ExpectNative<List<Token>> ();
            AnyObject body = term.GetProperty (Namespaces.Parser.Get ("body"));

            SyntaxParser parser = new SyntaxParser (this, syntaxTokens, Parsers);
            Syntax syntax = parser.ParseSyntax ();
            AddSyntax (syntax);

            // if its a macro (isQuote == true) then we add its transform function
            // if its just syntax, and the body it non-empty, then we add the syntaxes transform function
            if (isQuote || body.Count > 0) {
                AnyObject bodyStatementBlock = ExpressionCompiler.BuildSubExpressionBlock (body, term.SourceLocation);
                AddMacro (syntax.Name, syntax.Productions.Select (p => p.Name), term, bodyStatementBlock, isQuote);
            }
        }
    }*/
}
