using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Tycho.Compiler;
//using Tycho.Lexer;
using Tycho.Parser;
using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Language {
    /*public class ParserLanguage {
        class ParsedParser {
            public string Name { get; set; }
            public List<Token> Tokens { get; set; }

            public ParsedParser () {
                Tokens = new List<Token> ();
            }
        }

        public IParser MacroParser { get; set; }
        public IParser TokensParser { get; set; }
        public MacroBuilder MacroBuilder { get; set; }

        public Dictionary<string, IParser> BuildParsers (string parserSource) {
            Dictionary<string, IParser> parsers = new Dictionary<string, IParser> ();

            if (TokensParser != null) {
                parsers.Add (TokensParser.Name, TokensParser);
            }

            List<ParsedParser> parsedParsers = ParseParsers (NativeLexer.Lex (parserSource));

            foreach (ParsedParser parsedParser in parsedParsers) {
                MacroParser parser = new MacroParser (parsedParser.Name, parsers, MacroBuilder);
                parser.SetupMacroSyntax (MacroParser, TokensParser);
                parsers.Add (parsedParser.Name, parser);
            }

            foreach (ParsedParser parsedParser in parsedParsers) {
                SyntaxLoader.Configure ((MacroParser) parsers [parsedParser.Name], parsedParser.Tokens, parsers);
            }

            return parsers;
        }

        List<ParsedParser> ParseParsers (List<Token> tokens) {
            List<ParsedParser> parsers = new List<ParsedParser> ();

            int n = 0;
            while (n < tokens.Count) {
                if (n > 0) {
                    n++;

                    if (n == tokens.Count) {
                        break;
                    }

                    IdentifierToken newLineToken = tokens [n] as IdentifierToken;
                    if (newLineToken == null || newLineToken.Identifier != ";") {
                        throw new CompilationException (@"expected ""=""", tokens [n].SourceLocation);
                    }

                    n++;
                }

                ParsedParser parser = new ParsedParser ();

                IdentifierToken parserToken = tokens [n] as IdentifierToken;
                if (parserToken == null || parserToken.Identifier != "parser") {
                    throw new CompilationException (@"expected ""parser""", tokens [n].SourceLocation);
                }

                n++;
                IdentifierToken parserName = tokens [n] as IdentifierToken;

                if (parserName == null) {
                    throw new CompilationException ("expected identifier for syntax name", tokens [n].SourceLocation);
                }

                parser.Name = parserName.Identifier;

                n++;
                BracketToken parserBodyToken = tokens [n] as BracketToken;
                if (parserBodyToken == null || parserBodyToken.BracketType != BracketType.Brace) {
                    throw new CompilationException (@"expected { syntax ... }", tokens [n].SourceLocation);
                }

                parser.Tokens = parserBodyToken.Tokens;
                parsers.Add (parser);
            }

            return parsers;
        }
    }*/
}
