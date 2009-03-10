using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Parser.Tokens;

namespace Tycho.Parser {
    /*class NodePair {
        public Node First { get; private set; }
        public Node Last { get; private set; }

        public NodePair (Node first, Node last) {
            First = first;
            Last = last;
        }

        public TransitionNode LastTransitionNode {
            get { return (TransitionNode) Last; }
        }

        public TransitionNode FirstTransitionNode {
            get { return (TransitionNode) First; }
        }
    }

    public class ProductionLookup {
        Dictionary<AnyObject, ProductionItem> _productions = new Dictionary<AnyObject, ProductionItem> ();
        int ProductionIndex = 1;

        public int AddProduction (AnyObject name, IParser parser, bool singular, bool subParser) {
            ProductionItem production;
            if (_productions.TryGetValue (name, out production)) {
                // now there's more than one of these productions in the pattern, it ain't singular no more.
                production.Singular = false;
                return production.Index;
            } else {
                _productions [name] = new ProductionItem (name, parser, singular, ProductionIndex, subParser);
                return ProductionIndex++;
            }
        }

        public IEnumerable<ProductionItem> Productions {
            get { return _productions.Values; }
        }
    }

    public abstract class Pattern {
        public List<Pattern> Patterns { get; private set; }

        public Pattern () {
            Patterns = new List<Pattern> ();
        }

        public static Pattern Sequence (AnyObject name, bool singleToken, params Pattern [] patterns) {
            SequencePatterns (patterns);

            patterns [patterns.Length - 1].Append (new FinishPattern (new Production (name, Syntax.GetProductions (patterns)), singleToken));

            return patterns [0];
        }

        public static Pattern Sequence (params Pattern [] patterns) {
            SequencePatterns (patterns);

            return patterns [0];
        }

        public static void SequencePatterns (Pattern [] patterns) {
            for (int n = 0; n < patterns.Length - 1; n++) {
                patterns [n].Append (patterns [n + 1]);
            }
        }

        public void Append (Pattern pattern) {
            if (Patterns.Count > 0) {
                Pattern lastPattern = Patterns [Patterns.Count - 1];

                if (lastPattern.PatternEquals (pattern)) {
                    foreach (Pattern p in pattern.Patterns) {
                        lastPattern.Append (p);
                    }
                } else {
                    Patterns.Add (pattern);
                }
            } else {
                Patterns.Add (pattern);
            }
        }

        internal abstract void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions);

        internal void AnalyseAllProductions (bool singular, bool subParser, ProductionLookup productions) {
            AnalyseProductions (singular, subParser, productions);

            foreach (Pattern pattern in Patterns) {
                pattern.AnalyseAllProductions (singular, subParser, productions);
            }
        }

        internal abstract bool PatternEquals (Pattern pattern);

        internal bool FullyEquals (Pattern otherPattern) {
            return PatternEquals (otherPattern)
                && Patterns.Count == 1
                && otherPattern.Patterns.Count == 1
                && Patterns [0].PatternEquals (otherPattern.Patterns [0]);
        }

        public Node Compile () {
            return CompileAll ().First;
        }

        internal NodePair CompileAll () {
            NodePair nodes = CompileNodes ();
            Node lastNode = nodes.Last;

            foreach (Pattern pattern in Patterns) {
                NodePair patternNodes = pattern.CompileAll ();
                nodes.LastTransitionNode.Transitions.Add (new FreeTransition (patternNodes.First));
                lastNode = patternNodes.Last;
            }

            return new NodePair (nodes.First, lastNode);
        }


        internal abstract NodePair CompileNodes ();
    }

    public class ProductionPattern : Pattern {
        public AnyObject Name { get; private set; }
        public IParser Parser { get; set; }
        public bool Greedy { get; set; }
        public int Index { get; private set; }

        public ProductionPattern (AnyObject name, IParser parser, bool greedy) {
            Name = name;
            Parser = parser;
            Greedy = greedy;
            Index = 0;
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            Index = productions.AddProduction (Name, Parser, singular, subParser);
        }

        internal override bool PatternEquals (Pattern obj) {
            if (obj is ProductionPattern) {
                ProductionPattern otherPattern = (ProductionPattern) obj;
                return Index.Equals (otherPattern.Index) && Greedy.Equals (otherPattern.Greedy) && Parser == otherPattern.Parser;
            } else {
                return false;
            }
        }

        internal override NodePair CompileNodes () {
            TransitionNode firstNode = new TransitionNode (),
                remainingNode = new TransitionNode(),
                finalNode = new TransitionNode ();

            firstNode.Transitions.Add (new AnyTokenTransition (remainingNode, Index));

            var repeatTransition = new AnyTokenTransition (remainingNode, Index);
            var completeTransition = new FreeTransition (finalNode);

            if (Greedy) {
                remainingNode.Transitions.Add (repeatTransition);
                remainingNode.Transitions.Add (completeTransition);
            } else {
                remainingNode.Transitions.Add (completeTransition);
                remainingNode.Transitions.Add (repeatTransition);
            }

            return new NodePair (firstNode, finalNode);
        }
    }

    internal class FinishPattern : Pattern {
        public Production Production { get; private set; }
        bool SingleToken;

        public FinishPattern (Production production, bool singleToken) {
            Production = production;
            SingleToken = singleToken;
        }

        internal override bool PatternEquals (Pattern pattern) {
            return false;
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) { }

        internal override NodePair CompileNodes () {
            TransitionNode startNode = new TransitionNode ();
            FinishNode finishNode = new FinishNode (Production, SingleToken);

            startNode.Transitions.Add (new FinishTransition (finishNode));

            return new NodePair (startNode, finishNode);
        }
    }

    public class KeywordPattern : Pattern {
        public string Keyword { get; private set; }

        public KeywordPattern (string keyword) {
            Keyword = keyword;
        }

        internal override bool PatternEquals (Pattern pattern) {
            return pattern is KeywordPattern && Keyword.Equals (((KeywordPattern) pattern).Keyword);
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) { }

        internal override NodePair CompileNodes () {
            TransitionNode startNode = new TransitionNode (),
                endNode = new TransitionNode ();

            startNode.Transitions.Add (new KeywordTransition (endNode, Keyword));

            return new NodePair (startNode, endNode);
        }
    }

    public class DualKeywordPattern : KeywordPattern {
        public string OtherKeyword { get; private set; }

        public DualKeywordPattern (string keyword, string otherKeyword)
            : base (keyword) {
            OtherKeyword = otherKeyword;
        }

        internal override bool PatternEquals (Pattern pattern) {
            return pattern is DualKeywordPattern && Keyword.Equals (((DualKeywordPattern) pattern).Keyword) && OtherKeyword.Equals (((DualKeywordPattern) pattern).OtherKeyword);
        }

        internal override NodePair CompileNodes () {
            TransitionNode startNode = new TransitionNode (),
                endNode = new TransitionNode ();

            startNode.Transitions.Add (new KeywordTransition (endNode, Keyword));
            startNode.Transitions.Add (new KeywordTransition (endNode, OtherKeyword));

            return new NodePair (startNode, endNode);
        }
    }

    public class PlusPattern : KleenePattern {
        public PlusPattern (Pattern pattern, bool greedy) : base (pattern, greedy) { }

        internal override bool PatternEquals (Pattern obj) {
            if (obj is PlusPattern) {
                return base.PatternEquals (obj);
            } else {
                return false;
            }
        }

        internal override NodePair CompileNodes () {
            NodePair patternNodes = Pattern.CompileAll ();
            NodePair kleeneNodes = base.CompileNodes ();

            patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (kleeneNodes.First));

            return new NodePair (patternNodes.First, kleeneNodes.Last);
        }
    }

    public class KleenePattern : Pattern {
        protected Pattern Pattern { get; set; }
        bool Greedy { get; set; }

        public KleenePattern (Pattern pattern, bool greedy) {
            Pattern = pattern;
            Greedy = greedy;
        }

        internal override bool PatternEquals (Pattern obj) {
            if (obj is PlusPattern) {
                PlusPattern otherPattern = (PlusPattern) obj;
                return Pattern.FullyEquals (otherPattern.Pattern);
            } else {
                return false;
            }
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            Pattern.AnalyseAllProductions (false, subParser, productions);
        }

        internal override NodePair CompileNodes () {
            TransitionNode finalNode = new TransitionNode ();

            NodePair patternNodes = Pattern.CompileAll ();

            var repeatTransition = new FreeTransition (patternNodes.First);
            var completeTransition = new FreeTransition (finalNode);

            if (Greedy) {
                patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (patternNodes.First));
                patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (finalNode));
            } else {
                patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (finalNode));
                patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (patternNodes.First));
            }

            return new NodePair (patternNodes.First, finalNode);
        }
    }

    public class OnePattern : Pattern {
        public AnyObject Name { get; private set; }
        public IParser Parser { get; private set; }
        public int Index { get; private set; }

        public OnePattern (AnyObject name, IParser parser) {
            Name = name;
            Parser = parser;
            Index = 0;
        }

        internal override bool PatternEquals (Pattern obj) {
            return obj is OnePattern && Name.Equals (((OnePattern) obj).Name);
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            Index = productions.AddProduction (Name, Parser, singular, subParser);
        }

        internal override NodePair CompileNodes () {
            TransitionNode firstNode = new TransitionNode (), lastNode = new TransitionNode ();

            firstNode.Transitions.Add (new AnyTokenTransition (lastNode, Index));

            return new NodePair (firstNode, lastNode);
        }
    }

    public class BracketPattern : Pattern {
        BracketType BracketType { get; set; }
        Pattern Pattern { get; set; }

        public BracketPattern (Pattern pattern, BracketType type) {
            Pattern = pattern;
            BracketType = type;
        }

        internal override bool PatternEquals (Pattern obj) {
            return obj is BracketPattern && BracketType == ((BracketPattern) obj).BracketType && Pattern.FullyEquals (((BracketPattern) obj).Pattern);
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            Pattern.AnalyseAllProductions (singular, true, productions);
        }

        internal override NodePair CompileNodes () {
            TransitionNode first = new TransitionNode (), last = new TransitionNode ();

            first.Transitions.Add (new BracketTransition (last, Pattern.Compile (), BracketType));

            return new NodePair (first, last);
        }
    }

    public class DelimitedPattern : Pattern {
        Pattern Pattern { get; set; }
        Pattern Delimiter { get; set; }

        public DelimitedPattern (Pattern pattern, Pattern delimiter) {
            Pattern = pattern;
            Delimiter = delimiter;
        }

        internal override bool PatternEquals (Pattern obj) {
            return obj is DelimitedPattern
                && Pattern.FullyEquals (((DelimitedPattern) obj).Pattern)
                && Delimiter.FullyEquals (((DelimitedPattern) obj).Delimiter);
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            Pattern.AnalyseAllProductions (false, subParser, productions);
        }

        internal override NodePair CompileNodes () {
            TransitionNode startNode = new TransitionNode ();
            NodePair patternNodes = Pattern.CompileAll ();
            NodePair delimiterNodes = Delimiter.CompileAll ();
            TransitionNode finalNode = new TransitionNode ();

            startNode.Transitions.Add (new FreeTransition (patternNodes.First));
            patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (delimiterNodes.First));
            delimiterNodes.LastTransitionNode.Transitions.Add (new FreeTransition (patternNodes.First));

            // match optional trailing delimiter, eg: "a, b, c," ignores the last ","
            delimiterNodes.LastTransitionNode.Transitions.Add (new FreeTransition (finalNode));

            patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (finalNode));

            // for matching nothing
            startNode.Transitions.Add (new FreeTransition (finalNode));

            return new NodePair (startNode, finalNode);
        }
    }

    public class OptionalPattern : Pattern {
        Pattern Pattern { get; set; }

        public OptionalPattern (Pattern pattern) {
            Pattern = pattern;
        }

        internal override bool PatternEquals (Pattern obj) {
            return obj is OptionalPattern && Pattern.FullyEquals (((OptionalPattern) obj).Pattern);
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            Pattern.AnalyseAllProductions (singular, subParser, productions);
        }

        internal override NodePair CompileNodes () {
            TransitionNode startNode = new TransitionNode (), endNode = new TransitionNode ();

            NodePair patternNodes = Pattern.CompileAll ();
            startNode.Transitions.Add (new FreeTransition (patternNodes.First));
            startNode.Transitions.Add (new FreeTransition (endNode));
            patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (endNode));

            return new NodePair (startNode, endNode);
        }
    }

    public class AlternativePattern : Pattern {
        public List<Pattern> Alternatives { get; set; }

        public AlternativePattern () {
            Alternatives = new List<Pattern> ();
        }

        internal override bool PatternEquals (Pattern obj) {
            return false;
        }

        internal override void AnalyseProductions (bool singular, bool subParser, ProductionLookup productions) {
            foreach (Pattern pattern in Alternatives) {
                pattern.AnalyseAllProductions (singular, subParser, productions);
            }
        }

        internal override NodePair CompileNodes () {
            TransitionNode startNode = new TransitionNode (), endNode = new TransitionNode ();

            foreach (Pattern pattern in Alternatives) {
                NodePair patternNodes = pattern.CompileAll ();
                startNode.Transitions.Add (new FreeTransition (patternNodes.First));
                patternNodes.LastTransitionNode.Transitions.Add (new FreeTransition (endNode));
            }

            return new NodePair (startNode, endNode);
        }
    }*/
}
