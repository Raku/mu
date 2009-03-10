using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using Tycho.Compiler;
using Tycho.Runtime;
//using Tycho.Lexer;
using Tycho.Parser.Tokens;
using System.IO;
using System.Text.RegularExpressions;
using System.Reflection;

namespace Tycho.Parser {
    /*public class Parser : IParser {
        public string Name { get; private set; }
        Node FirstNode;
        List<Pattern> Patterns;
        protected List<Pattern> HighPrecedencePatterns;
        Parser ParentParser;
        public const string StatementDelimiter = ";";
        Dictionary<AnyObject, Syntax> Syntaxes;

        public Parser (string name) : this (name, null) { }

        public Parser (string name, Parser parentParser) {
            Name = name;
            Patterns = new List<Pattern> ();
            HighPrecedencePatterns = new List<Pattern> ();
            ParentParser = parentParser;
            Syntaxes = new Dictionary<AnyObject, Syntax> ();
        }

        public virtual IParser ExtendParser () {
            return new Parser (Name, this);
        }

        Node ParseNode {
            get {
                if (FirstNode == null) {
                    FirstNode = CompileFsa ();
                }

                return FirstNode;
            }
        }

        public void AddSyntax (Syntax syntax) {
            if (Syntaxes.ContainsKey (syntax.Name)) {
                throw new SyntaxErrorException(String.Format ("syntax `{0}' already defined", syntax.Name));
            }

            Syntaxes.Add (syntax.Name, syntax);
            Pattern pattern = syntax.Pattern;

            if (Patterns.Count > 0 && Patterns [Patterns.Count - 1].Equals (pattern)) {
                Patterns [Patterns.Count - 1].Append (pattern);
            } else {
                Patterns.Add (pattern);
            }

            FirstNode = null;
        }

        public AnyObject Parse (List<Token> tokens) {
            MatchResult result = Match (tokens, 0);
            AnyObject term = ConstructTerm (result, tokens, 0);

            if (result != null && result.FinishIndex < tokens.Count) {
                AnyObject list = CompilerModule.CreateTermList ();
                list.Add (term);

                ParseLines (tokens, result.FinishIndex + 1, list);

                return list;
            } else {
                return term;
            }
        }

        MatchResult Match (List<Token> tokens, int startIndex) {
            Node node = ParseNode;

            if (node != null) {
                MatchResult result = node.Match (tokens, startIndex, new HashSet<NodeIndex> ());
                if (result != null) {
                    return result;
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }

        void ParseLines (List<Token> tokens, int startIndex, AnyObject list) {
            if (startIndex < tokens.Count) {
                do {
                    MatchResult result = Match (tokens, startIndex);

                    list.Add (ConstructTerm (result, tokens, startIndex));

                    if (result != null && result.FinishIndex < tokens.Count) {
                        startIndex = result.FinishIndex + 1;
                    } else {
                        break;
                    }
                } while (true);
            }
        }

        protected virtual AnyObject ConstructTerm (MatchResult result, List<Token> tokens, int startIndex) {
            if (result != null) {
                return result.BuildTerm (this, tokens [startIndex].SourceLocation.RangeWith (tokens [result.FinishIndex - 1].SourceLocation));
            } else if (tokens.Count - startIndex == 1) {
                return ParseToken (tokens [0], true);
            } else {
                return ParseError (tokens.GetRange (startIndex, tokens.Count - startIndex));
            }
        }

        public AnyObject ParseToken (Token token, bool interpolateString) {
            if (token is IdentifierToken) {
                IdentifierToken id = (IdentifierToken) token;
                return Terms.FromIdentifier (id.Identifier, id.ModulePath, id.SourceLocation);
            } else if (token is StringToken) {
                return ParseStringToken (token, interpolateString);
            } else if (token is IntegerToken) {
                return Terms.FromInteger (((IntegerToken) token).Value, token.SourceLocation);
            } else if (token is FloatToken) {
                return Terms.FromReal (((FloatToken) token).Value, token.SourceLocation);
            } else {
                return Terms.FromError (token.ToString (), token.SourceLocation);
            }
        }

        private AnyObject ParseStringToken (Token token, bool interpolateString) {
            StringToken stringToken = (StringToken) token;

            var value = stringToken.UnescapedValue;
            if (interpolateString && stringToken.Escaped) {
                var tokens = NativeLexer.InterpolateString (stringToken.Value, token.SourceLocation);
                if (tokens.Count == 1) {
                    return Terms.FromString (value, token.SourceLocation);
                }
                return Terms.FromInterpolatedString (tokens.Select (t => Parse (new List<Token> {t})), stringToken.SourceLocation);
            } else {
                return Terms.FromString (value, token.SourceLocation);
            }
        }

        AnyObject ParseError (List<Token> tokens) {
            if (tokens.Count == 0) return "";
            var sloc = tokens [0].SourceLocation.RangeWith (tokens [tokens.Count - 1].SourceLocation);
            AnyObject list = CompilerModule.CreateTermList ();
            tokens.ForEach ((token) => list.Add (ParseToken (token, false)));
            AnyObject str = CompilerModule.CreateTerm (sloc, Symbols.ParserError, list);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserError);
            return str;
        }

        public AnyObject Parse (List<List<Token>> tokensList) {
            AnyObject list = CompilerModule.CreateTermList ();

            foreach (List<Token> tokens in tokensList) {
                list.Add (Parse (tokens));
            }

            return list;
        }

        Node CompileFsa () {
            Node lastNode = null, firstNode = null;

            foreach (Pattern pattern in Patterns.Concat (HighPrecedencePatterns)) {
                Node node = pattern.Compile ();

                if (firstNode == null) {
                    firstNode = node;
                }

                if (lastNode != null) {
                    ((TransitionNode) lastNode).Transitions.Add (new FreeTransition (node));
                }

                lastNode = node;
            }

            if (ParentParser != null) {
                if (lastNode != null) {
                    ((TransitionNode) lastNode).Transitions.Add (new FreeTransition (ParentParser.ParseNode));
                } else {
                    return ParentParser.ParseNode;
                }
            }

            return firstNode;
        }
    }*/
}
