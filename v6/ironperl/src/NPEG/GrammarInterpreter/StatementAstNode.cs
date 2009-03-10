#region License
/* **********************************************************************************
 * Copyright (c) Leblanc Meneses
 * This source code is subject to terms and conditions of the MIT License
 * for NPEG. A copy of the license can be found in the License.txt file
 * at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * MIT License.
 * You must not remove this notice from this software.
 * **********************************************************************************/
#endregion
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;


namespace NPEG.GrammarInterpreter
{
    public class StatementAstNode : IAstNodeReplacement
    {
        public Stack<AExpression> stackBinary = new Stack<AExpression>();

        public StatementAstNode()
        {
        }


        Boolean isNodeDefinition = false;
        public override void VisitEnter(AstNode node)
        {
#if (DIAGNOSTICS)
            System.Diagnostics.Debug.WriteLine("VisitEnter: " + node.Token.Name + " " + node.Token.Value);
            System.Diagnostics.Debug.Indent();
#endif            
            // needed for reference terminals
            switch (node.Token.Name)
            {
                case "NodeDefinition":
                    if (node.Children.Count > 0)
                    //(?<ThreeDigitCode>): *****;
                    {
                        this.NodeDefinition = node.Children[0].Children[0].Token.Value;
                    }
                    else
                    //PhoneNumber: *****;
                    {
                        this.NodeDefinition = node.Token.Value;
                    }
                    break;
                default: break;
            }
            

            // it will transverse the tree again once the complete AST has been created.
            if (this.Parent == null)
                return;


            switch (node.Token.Name)
            {
                case "NodeDefinition":
                    // needed for capturing group
                    this.isNodeDefinition = true;
                    break;
                default: break;
            }
        }


        public override void VisitExecute(AstNode node)
        {
#if (DIAGNOSTICS)
            System.Diagnostics.Debug.WriteLine("VisitExecute: " + node.Token.Name + " " + node.Token.Value);
#endif
        }



        public override void VisitLeave(AstNode node)
        {
#if (DIAGNOSTICS)
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: " + node.Token.Name + " " + node.Token.Value);
#endif

            // it will transverse the tree again once the complete AST has been created.
            if (this.Parent == null)
                return;

            AExpression left;
            AExpression right;

            switch (node.Token.Name)
            {
                case "NodeDefinition":
                    // needed for capturing group
                    this.isNodeDefinition = false;
                    break;
                // for binary operators pop left and right
                case "Statement":
                    this.expression = this.stackBinary.Pop();
                    break;
                case "Sequence":
                    Stack<AExpression> reverse = new Stack<AExpression>();
                    for (int i = 0; i < node.Children.Count; i++)
                    {
                        reverse.Push(this.stackBinary.Pop());
                    }

                    Decimal sequence_cnt = (decimal)node.Children.Count - 1;
                    for(; sequence_cnt > 0; sequence_cnt--)
                    {
                        left = reverse.Pop();
                        right = reverse.Pop();
                        reverse.Push( 
                            new NonTerminals.Sequence(left, right)
                        );
                    }

                    this.stackBinary.Push(reverse.Pop());

                    break;
                case "PrioritizedChoice":
                    right = this.stackBinary.Pop();
                    left = this.stackBinary.Pop();

                    this.stackBinary.Push(
                        new NonTerminals.PrioritizedChoice(left, right)
                    );
                    break;





                case "Prefix":
                    switch (node.Token.Value[0].ToString())
                    {
                        case "!":
                            this.stackBinary.Push(new NonTerminals.NotPredicate(this.stackBinary.Pop()));
                            break;
                        case "&":
                            this.stackBinary.Push(new NonTerminals.AndPredicate(this.stackBinary.Pop()));
                            break;
                        default:
                            throw new Exception("Unsupported PEG Prefix.");
                    }
                    break;

                case "Suffix":
                    switch (node.Children[1].Token.Name)
                    {
                        case "ZeroOrMore":
                            this.stackBinary.Push(new NonTerminals.ZeroOrMore(this.stackBinary.Pop()));
                            break;
                        case "OneOrMore":
                            this.stackBinary.Push(new NonTerminals.OneOrMore(this.stackBinary.Pop()));
                            break;
                        case "Optional":
                            this.stackBinary.Push(new NonTerminals.Optional(this.stackBinary.Pop()));
                            break;
                        case "LimitingRepetition":
                            switch (node.Children[1].Children[0].Token.Name)
                            {
                                case "MIN_MAX":
                                    this.stackBinary.Push(new NonTerminals.LimitingRepetition(this.stackBinary.Pop()) { Min = Int32.Parse(node.Children[1].Children[0].Children[0].Token.Value), Max = Int32.Parse(node.Children[1].Children[0].Children[1].Token.Value) });
                                    break;
                                case "_MAX":
                                    this.stackBinary.Push(new NonTerminals.LimitingRepetition(this.stackBinary.Pop()) { Min = null, Max = Int32.Parse(node.Children[1].Children[0].Children[0].Token.Value) });
                                    break;
                                case "MIN_":
                                    this.stackBinary.Push(new NonTerminals.LimitingRepetition(this.stackBinary.Pop()) { Min = Int32.Parse(node.Children[1].Children[0].Children[0].Token.Value), Max = null });
                                    break;
                                case "Digit":
                                    Int32 exactcount = Int32.Parse(node.Children[1].Children[0].Token.Value);
                                    this.stackBinary.Push(new NonTerminals.LimitingRepetition(this.stackBinary.Pop()) { Min = exactcount, Max = exactcount });
                                    break;
                                default: break;
                            }
                            break;
                        default:
                            throw new Exception("Unsupported PEG Suffix.");
                    }
                    break;




                case "CapturingGroup":
                    if (this.isNodeDefinition)
                    {
                        this.TerminalCapturingGroup = node;
                    }
                    else
                    {
                        #warning onmatchcreate astnode.
                        this.stackBinary.Push(new NonTerminals.CapturingGroup(node.Children[0].Token.Value, this.stackBinary.Pop()));
                    }
                    break;
                case "Group":
                    break;
                case "AnyCharacter":
                    this.stackBinary.Push(new Terminals.AnyCharacter());
                    break;
                case "Literal":
                    Boolean isCaseSensitive = true;
                    if (node.Children.Count == 2)
                        isCaseSensitive = false;

                    this.stackBinary.Push(new Terminals.Literal() { IsCaseSensitive = isCaseSensitive, MatchText = Regex.Replace(node.Children[0].Token.Value, @"\\(?<quote>""|')", @"${quote}") });
                    break;
                case "CharacterClass":
                    this.stackBinary.Push(new Terminals.CharacterClass() { ClassExpression = node.Token.Value });
                    break;
                case "TerminalReference":
                    this.stackBinary.Push(((Interpreter)this.Parent)[node.Token.Value]);
                    break;
                case "DynamicBackReferencing":
                    if (node.Children.Count == 1)
                    {
                        // no options specified only tag name.
                        this.stackBinary.Push(new Terminals.DynamicBackReference() { BackReferenceName = node.Children[0].Token.Value });
                    }
                    else
                    {
                        throw new NotImplementedException("Add IsCaseSensitive using children[1].Token.Name == IsCasesensitive");
                    }
                    break;
                default: break;
            }
        }


        private AstNode TerminalCapturingGroup;
        public String NodeDefinition
        {
            get;
            set;
        }



        private AExpression expression;
        public AExpression Expression
        {
            get 
            {
                if (expression == null)
                {

#if (DIAGNOSTICS)
                    System.Diagnostics.Debug.WriteLine("");
                    System.Diagnostics.Debug.WriteLine("");
                    System.Diagnostics.Debug.WriteLine("START - StatementAstNode.cs");
#endif

                    this.Accept(this);
                    // retransverse the tree but this time parent is available to share data among other branches.

#if (DIAGNOSTICS)
                    System.Diagnostics.Debug.WriteLine("END   - StatementAstNode.cs");
                    System.Diagnostics.Debug.WriteLine("");
                    System.Diagnostics.Debug.WriteLine("");
#endif
                }

                if (this.TerminalCapturingGroup != null)
                {
                    this.expression = new NPEG.NonTerminals.CapturingGroup(this.TerminalCapturingGroup.Children[0].Token.Value, this.expression);
                }

                return this.expression;
            }
        }

        public override void Dispose()
        {

        }
    }
}
