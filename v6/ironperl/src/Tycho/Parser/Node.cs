using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Compiler;
using Tycho.Runtime;
using Tycho.Parser.Tokens;

namespace Tycho.Parser {
    /*class NodeIndex {
        Node Node;
        int Index;

        public NodeIndex (Node node, int index) {
            Node = node;
            Index = index;
        }

        public override bool Equals (object obj) {
            if (obj is NodeIndex) {
                NodeIndex ni = (NodeIndex) obj;

                return ni.Node == Node && ni.Index == Index;
            } else {
                return false;
            }
        }

        public override int GetHashCode () {
            return Node.GetHashCode () + Index.GetHashCode ();
        }
    }

    public class ProductionItem {
        public AnyObject Name { get; set; }
        public IParser Parser { get; set; }
        public bool Singular { get; set; }
        public int Index { get; set; }
        public bool SubParser { get; set; }

        public ProductionItem (AnyObject name, IParser parser, bool singular, int index, bool subParser) {
            Name = name;
            Parser = parser;
            Singular = singular;
            Index = index;
            SubParser = subParser;
        }
    }

    public class Production {
        AnyObject Name;
        Dictionary<int, ProductionItem> Names = new Dictionary<int, ProductionItem> ();

        public Production (AnyObject name, IEnumerable<ProductionItem> productions) {
            Name = name;
            foreach (ProductionItem production in productions) {
                Names [production.Index] = production;
            }
        }

        public AnyObject BuildTerm (Dictionary<int, List<List<Token>>> results, SourceLocation sourceLocation) {
            AnyObject term = CompilerModule.CreateTerm (sourceLocation);

            if (Name != null) {
                term.SetProperty (Symbols.ParserTermName, Name);
            }

            foreach (KeyValuePair<int, ProductionItem> name in Names) {
                if (results.ContainsKey (name.Key)) {
                    AnyObject parseResult;

                    IParser parser = name.Value.Parser;

                    if (name.Value.SubParser) {
                        parser = parser.ExtendParser ();
                    }

                    if (name.Value.Singular) {
                        parseResult = parser.Parse (results [name.Key] [0]);
                    } else {
                        parseResult = parser.Parse (results [name.Key]);
                    }

                    term.SetProperty (name.Value.Name, parseResult);
                } else if (!name.Value.Singular) {
                    term.SetProperty (name.Value.Name, CompilerModule.CreateTermList ());
                }
            }

            return term;
        }
    }

    public class MatchResult {
        Dictionary<int, List<List<Token>>> Results = new Dictionary<int,List<List<Token>>>();
        int LastIndex = 0;
        Production Production;
        public int FinishIndex { get; private set; }
        bool SingleToken;

        public MatchResult (Production production, int finishIndex, bool singleToken) {
            Production = production;
            FinishIndex = finishIndex;
            SingleToken = singleToken;
        }

        public void AddToken (int index, Token token) {
            if (index != 0) {
                List<List<Token>> tokenListList;
                List<Token> tokenList;

                if (!Results.TryGetValue (index, out tokenListList)) {
                    tokenListList = new List<List<Token>> ();
                    Results [index] = tokenListList;

                    tokenList = new List<Token> ();
                    tokenListList.Insert (0, tokenList);
                } else if (LastIndex != index) {
                    tokenList = new List<Token> ();
                    tokenListList.Insert (0, tokenList);
                } else {
                    tokenList = tokenListList [0];
                }

                tokenList.Insert (0, token);
            }

            LastIndex = index;
        }

        public AnyObject BuildTerm (Parser parser, SourceLocation sourceLocation) {
            if (SingleToken) {
                return parser.ParseToken (Results [1] [0] [0], true);
            } else {
                return Production.BuildTerm (Results, sourceLocation);
            }
        }

        internal void MergeResults (MatchResult subMatch) {
            foreach (KeyValuePair<int, List<List<Token>>> item in subMatch.Results) {
                Results [item.Key] = item.Value;
            }
        }
    }

    public abstract class Node {
        internal abstract MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done);
    }

    public class TransitionNode : Node {
        internal List<Transition> Transitions { get; private set; }

        public TransitionNode () {
            Transitions = new List<Transition>();
        }

        internal override MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done) {
            NodeIndex nodeIndex = new NodeIndex (this, startIndex);

            if (done.Contains (nodeIndex)) {
                return null;
            }

            done.Add (nodeIndex);

            foreach (Transition t in Transitions) {
                MatchResult result = t.Match (tokens, startIndex, done);

                if (result != null) {
                    return result;
                }
            }

            return null;
        }
    }

    class FinishNode : Node {
        Production Production;
        bool SingleToken;

        public FinishNode (Production production, bool singleToken) {
            Production = production;
            SingleToken = singleToken;
        }

        internal override MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done) {
            return new MatchResult (Production, startIndex, SingleToken);
        }
    }

    abstract class Transition {
        public Node Node { get; protected set; }

        public Transition (Node node) {
            Node = node;
        }

        public abstract MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done);
    }

    abstract class TokenTransition : Transition {
        public int ResultIndex { get; protected set; }

        public TokenTransition (Node node, int resultIndex) : base (node) {
            ResultIndex = resultIndex;
        }

        public override MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done) {
            if (startIndex >= tokens.Count) {
                return null;
            }

            if (Match (tokens [startIndex])) {
                MatchResult result = Node.Match (tokens, startIndex + 1, done);

                if (result != null) {
                    result.AddToken (ResultIndex, tokens [startIndex]);
                    return result;
                }
            }

            return null;
        }

        public abstract bool Match (Token token);
    }

    class BracketTransition : Transition {
        Node SubNode;
        BracketType BracketType;

        public BracketTransition (Node nextNode, Node subNode, BracketType bracketType) : base (nextNode) {
            SubNode = subNode;
            BracketType = bracketType;
        }

        public override MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done) {
            if (startIndex >= tokens.Count) {
                return null;
            }

            MatchResult subMatch = Match (tokens [startIndex]);

            if (subMatch != null) {
                MatchResult result = Node.Match (tokens, startIndex + 1, done);

                if (result != null) {
                    result.MergeResults (subMatch);
                    return result;
                }
            }

            return null;
        }

        MatchResult Match (Token token) {
            if (token is BracketToken && ((BracketToken) token).BracketType == BracketType) {
                BracketToken bracketToken = (BracketToken) token;

                return SubNode.Match (bracketToken.Tokens, 0, new HashSet<NodeIndex> ());
            } else {
                return null;
            }
        }
    }

    class FreeTransition : Transition {
        public FreeTransition (Node node) : base (node) { }

        public override MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done) {
            return Node.Match (tokens, startIndex, done);
        }
    }

    class AnyTokenTransition : TokenTransition {
        public AnyTokenTransition (Node node, int resultIndex) : base (node, resultIndex) { }

        public override bool Match (Token token) {
            if (token is IdentifierToken) {
                IdentifierToken id = (IdentifierToken) token;

                return !(id.Identifier == ";" && id.ModulePath == null);
            } else {
                return true;
            }
        }
    }

    class KeywordTransition : TokenTransition {
        string Keyword, OtherKeyword;

        public KeywordTransition (Node node, string keyword, string otherKeyword)
            : base (node, 0) {
            Keyword = keyword;
            OtherKeyword = otherKeyword;
        }

        public KeywordTransition (Node node, string keyword) : this (node, keyword, null) { }

        public override bool Match (Token token) {
            if (token is IdentifierToken) {
                IdentifierToken id = (IdentifierToken) token;
                return id.ModulePath == null && id.Identifier == Keyword && (OtherKeyword == null || id.Identifier == OtherKeyword);
            } else {
                return false;
            }
        }
    }

    class FinishTransition : Transition {
        public FinishTransition (Node node) : base (node) { }

        public override MatchResult Match (List<Token> tokens, int startIndex, HashSet<NodeIndex> done) {
            if (startIndex == tokens.Count || (tokens [startIndex] is IdentifierToken && ((IdentifierToken) tokens [startIndex]).Identifier == ";")) {
                MatchResult result = Node.Match (tokens, startIndex, done);

                if (result != null) {
                    return result;
                }
            }

            return null;
        }
    }*/
}
