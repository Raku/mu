using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using Tycho.Parser.Tokens;
using System.Reflection;
using System.Text.RegularExpressions;
using Tycho.Runtime;
//using Tycho.Lexer;
using Tycho.Compiler;
using Tycho.Language;

namespace Tycho.Parser {
    /*public class Syntax {
        public Syntax (AnyObject name, bool singleToken, params Pattern [] patterns) {
            Name = name;
            Pattern.SequencePatterns (patterns);

            Productions = GetProductions (patterns);
            patterns [patterns.Length - 1].Append (new FinishPattern (new Production (name, Productions), singleToken));

            Pattern = patterns [0];
        }

        public AnyObject Name { get; private set; }
        public Pattern Pattern { get; private set; }
        public List<ProductionItem> Productions { get; private set; }

        public static List<ProductionItem> GetProductions (IEnumerable<Pattern> patterns) {
            ProductionLookup productionLookup = new ProductionLookup ();

            foreach (Pattern pattern in patterns) {
                pattern.AnalyseProductions (true, false, productionLookup);
            }

            return new List<ProductionItem> (productionLookup.Productions);
        }
    }

    public class SyntaxLoader {
        public static Dictionary<string, IParser> ConfigureParsers (AnyObject macroContext) {
            Assembly syntaxAssembly = typeof (Parser).Assembly;
            string [] resources = syntaxAssembly.GetManifestResourceNames ();
            Regex namePattern = new Regex (@"Tycho\.Parser\.Syntax\.([^.]*)\.syntax");

            Dictionary<string, IParser> parsers = new Dictionary<string, IParser> ();

            MacroBuilder macroBuilder = new MacroBuilder (macroContext);
            IParser tokensParser = new TokenListParser ();
            parsers.Add ("tokens", tokensParser);
            MacroParser expressionParser = new MacroParser ("expression", parsers, macroBuilder);
            expressionParser.SetupMacroSyntax (expressionParser, tokensParser);
            parsers.Add (expressionParser.Name, expressionParser);

            foreach (string name in resources) {
                Match m = namePattern.Match (name);

                if (m.Success) {
                    string parserName = m.Groups [1].Value;
                    if (parserName != expressionParser.Name) {
                        MacroParser parser = new MacroParser (parserName, parsers, macroBuilder);
                        parser.SetupMacroSyntax (expressionParser, tokensParser);
                        parsers.Add (parserName, parser);
                    }
                }
            }

            foreach (MacroParser parser in parsers.Values.Where (l => l is MacroParser)) {
                StreamReader file = new StreamReader (syntaxAssembly.GetManifestResourceStream ("Tycho.Parser.Syntax." + parser.Name + ".syntax"));
                Configure (parser, NativeLexer.Lex (file.ReadToEnd (), ""), parsers);
            }

            return parsers;
        }

        public static void Configure (MacroParser parser, List<Token> tokens, Dictionary<string, IParser> parsers) {
            SyntaxParser syntaxParser = new SyntaxParser (parser, tokens, parsers);
            List<UnparsedMacro> macros = new List<UnparsedMacro> ();

            do {
                Syntax syntax = syntaxParser.ParseSyntax ();
                parser.AddSyntax (syntax);

                UnparsedMacro macro;
                if (syntaxParser.TryParseMacro (syntax, out macro)) {
                    macros.Add (macro);
                }
            } while (syntaxParser.Index < tokens.Count);

            foreach (UnparsedMacro macro in macros) {
                SourceLocation sloc = macro.BodyTokens [0].SourceLocation.RangeWith (macro.BodyTokens [macro.BodyTokens.Count - 1].SourceLocation);
                AnyObject macroTerm = CompilerModule.CreateTerm (sloc);
                if (macro.BodyTokens.Count == 0) {
                    throw new ApplicationException ("macro has no body");
                }
                parser.AddMacro (macro.Name, macro.ProductionNames, macroTerm, parser.Parse (macro.BodyTokens), true);
            }

            parser.AddSyntax (new Syntax (Namespaces.Parser.Get ("token"), true, new OnePattern (Namespaces.Parser.Get ("token"), parser)));
        }

        public static string ParseName (Token token) {
            if (!(token is IdentifierToken)) throw new CompilationException ("syntax: syntax <name> <pattern>+", token.SourceLocation);
            return ((IdentifierToken) token).Identifier;
        }
    }

    class UnparsedMacro {
        public AnyObject Name;
        public IEnumerable<AnyObject> ProductionNames { get; private set; }
        public List<Token> BodyTokens { get; private set; }

        public UnparsedMacro (AnyObject name, IEnumerable<AnyObject> productionNames, List<Token> bodyTokens) {
            Name = name;
            ProductionNames = productionNames;
            BodyTokens = bodyTokens;
        }
    }

    class SyntaxParser {
        Dictionary<string, IParser> Parsers;
        List<Token> Tokens;
        public int Index { get; set; }
        IParser DefaultParser;

        public SyntaxParser (IParser defaultParser, List<Token> tokens, Dictionary<string, IParser> parsers) {
            DefaultParser = defaultParser;
            Index = 0;
            this.Tokens = tokens;
            this.Parsers = parsers;
        }

        public Syntax ParseSyntax () {
            List<Pattern> patterns = new List<Pattern> ();

            AnyObject name = Namespaces.Parser.Get (SyntaxLoader.ParseName (Tokens [Index]));

            Index++;
            ParseConfiguration (patterns, null);

            return new Syntax (name, false, patterns.ToArray ());
        }

        void ParseConfiguration (List<Pattern> patterns, string expectedBracket) {
            AlternativePattern altPattern = null;

            try {
                while (Index < Tokens.Count) {
                    Token token = Tokens [Index];

                    if (token is IdentifierToken) {
                        string id = ((IdentifierToken) token).Identifier;
                        switch (id) {
                            case "+": {
                                    if (patterns.Count == 0) throw new CompilationException ("+ operator requires previous pattern", token.SourceLocation);

                                    Pattern pattern = patterns [patterns.Count - 1];

                                    if (pattern is ProductionPattern) {
                                        ((ProductionPattern) pattern).Greedy = true;
                                    } else {
                                        patterns [patterns.Count - 1] = new PlusPattern (pattern, true);
                                    }
                                    break;
                                }
                            case "+?": {
                                    if (patterns.Count == 0) throw new CompilationException ("+? operator requires previous pattern", token.SourceLocation);

                                    patterns [patterns.Count - 1] = new PlusPattern (patterns [patterns.Count - 1], false);
                                    break;
                                }
                            case "*": {
                                    if (patterns.Count == 0) throw new CompilationException ("* operator requires previous pattern", token.SourceLocation);

                                    patterns [patterns.Count - 1] = new KleenePattern (patterns [patterns.Count - 1], true);
                                    break;
                                }
                            case "*?": {
                                    if (patterns.Count == 0) throw new CompilationException ("*? operator requires previous pattern", token.SourceLocation);

                                    patterns [patterns.Count - 1] = new KleenePattern (patterns [patterns.Count - 1], false);
                                    break;
                                }
                            case "...": {
                                    if (patterns.Count < 2) throw new CompilationException ("... syntax: <pattern> <keyword> ...", token.SourceLocation);

                                    Pattern repeatedPattern = patterns [patterns.Count - 2];
                                    Pattern delimiter = patterns [patterns.Count - 1];

                                    patterns.RemoveAt (patterns.Count - 1);
                                    patterns [patterns.Count - 1] = new DelimitedPattern (repeatedPattern, delimiter);

                                    break;
                                }
                            case "::": {
                                    Exception syntaxError = new ApplicationException (":: syntax: <name> :: <parser>");
                                    if (patterns.Count < 1) throw syntaxError;

                                    Pattern patternName = patterns [patterns.Count - 1];

                                    if (!(patternName is ProductionPattern)) throw syntaxError;
                                    if (Index + 1 >= Tokens.Count) throw syntaxError;
                                    if (!(Tokens [Index + 1] is IdentifierToken)) throw syntaxError;

                                    Index++;
                                    string parserName = ((IdentifierToken) Tokens [Index]).Identifier;

                                    ((ProductionPattern) patternName).Parser = Parsers [parserName];

                                    break;
                                }
                            case ";":
                                Index++;
                                return;
                            case "|": {
                                    if (altPattern == null) {
                                        altPattern = new AlternativePattern ();
                                    }

                                    altPattern.Alternatives.Add (Pattern.Sequence (patterns.ToArray ()));
                                    patterns.Clear ();
                                }
                                break;
                            default:
                                patterns.Add (new ProductionPattern (Namespaces.Parser.Get (id), DefaultParser, false));
                                break;
                        }
                    } else if (token is StringToken) {
                        string keyword = ((StringToken) token).Value;

                        switch (keyword) {
                            case "{": {
                                    Index++;
                                    List<Pattern> subPatterns = new List<Pattern> ();
                                    ParseConfiguration (subPatterns, "}");
                                    patterns.Add (new BracketPattern (new Syntax (null, false, subPatterns.ToArray ()).Pattern, BracketType.Brace));
                                    break;
                                }
                            case "(": {
                                    Index++;
                                    List<Pattern> subPatterns = new List<Pattern> ();
                                    ParseConfiguration (subPatterns, ")");
                                    patterns.Add (new BracketPattern (new Syntax (null, false, subPatterns.ToArray ()).Pattern, BracketType.Parenthesis));
                                    break;
                                }
                            case "[": {
                                    Index++;
                                    List<Pattern> subPatterns = new List<Pattern> ();
                                    ParseConfiguration (subPatterns, "]");
                                    patterns.Add (new BracketPattern (new Syntax (null, false, subPatterns.ToArray ()).Pattern, BracketType.Bracket));
                                    break;
                                }
                            case "}":
                            case ")":
                            case "]":
                                if (keyword != expectedBracket) throw new CompilationException ("expected " + expectedBracket, token.SourceLocation);
                                return;
                            case ",;":
                                patterns.Add (new DualKeywordPattern (",", ";"));
                                break;
                            default:
                                patterns.Add (new KeywordPattern (((StringToken) token).Value));
                                break;
                        }
                    } else if (token is BracketToken) {
                        switch (((BracketToken) token).BracketType) {
                            case BracketType.Bracket: {
                                    List<Pattern> subPatterns = new List<Pattern> ();
                                    SyntaxParser subParser = new SyntaxParser (DefaultParser, ((BracketToken) token).Tokens, Parsers);
                                    subParser.ParseConfiguration (subPatterns, null);

                                    patterns.Add (new OptionalPattern (Pattern.Sequence (subPatterns.ToArray ())));
                                    break;
                                }
                            case BracketType.Parenthesis: {
                                    List<Pattern> subPatterns = new List<Pattern> ();
                                    SyntaxParser subParser = new SyntaxParser (DefaultParser, ((BracketToken) token).Tokens, Parsers);
                                    subParser.ParseConfiguration (subPatterns, null);

                                    patterns.Add (Pattern.Sequence (subPatterns.ToArray ()));
                                    break;
                                }
                            case BracketType.Brace:
                                return;
                        }
                    } else if (token is IntegerToken && ((IntegerToken) token).Value == 1) {
                        Exception syntaxError = new ApplicationException ("1 syntax: 1 <name>");

                        if (Index + 1 >= Tokens.Count) throw syntaxError;
                        if (!(Tokens [Index + 1] is IdentifierToken)) throw syntaxError;

                        Index++;
                        string parserName = ((IdentifierToken) Tokens [Index]).Identifier;

                        patterns.Add(new OnePattern (Namespaces.Parser.Get (parserName), DefaultParser));
                    } else {
                        throw new CompilationException ("token not recognised: " + token, token.SourceLocation);
                    }

                    Index++;
                }
            } finally {
                if (altPattern != null) {
                    altPattern.Alternatives.Add (Pattern.Sequence (patterns.ToArray ()));
                    patterns.Clear ();
                    patterns.Add (altPattern);
                }
            }
        }

        public bool TryParseMacro (Syntax syntax, out UnparsedMacro macro) {
            if (Index < Tokens.Count) {
                Token token = Tokens [Index];

                if (token is BracketToken && ((BracketToken) token).BracketType == BracketType.Brace) {
                    IEnumerable<AnyObject> productionNames = syntax.Productions.Select (p => p.Name);
                    macro = new UnparsedMacro (syntax.Name, productionNames, ((BracketToken) token).Tokens);

                    Index++;
                    if (Index < Tokens.Count - 1) {
                        Token t = Tokens [Index];
                        if (t is IdentifierToken && ((IdentifierToken) t).Identifier == ";") {
                            Index++;
                        } else {
                            throw new CompilationException ("macro syntax is {};", t.SourceLocation);
                        }
                    }

                    return true;
                } else {
                    macro = null;
                    return false;
                }
            } else {
                macro = null;
                return false;
            }
        }
    }*/
}
