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

namespace NPEG
{    
    // write rules for existing composite
    public class CompositeGrammarCreatorVisitor : ICompositeGrammarVisitor
    {
        List<String> statements = new List<string>();
        Stack<StringBuilder> terminal = new Stack<StringBuilder>();    
        
        public CompositeGrammarCreatorVisitor()
        { 
        }

        public override void Dispose()
        {
        }

        public override void Visit(NPEG.Terminals.AnyCharacter expression)
        {
            terminal.Peek().Append('.');
        }
        public override void Visit(NPEG.Terminals.CharacterClass expression)
        {
            terminal.Peek().Append(expression.ClassExpression);
        }
        public override void Visit(NPEG.Terminals.Literal expression)
        {
            terminal.Peek().Append("'");
            terminal.Peek().Append(Regex.Replace(expression.MatchText, @"'", @"\'"));
            terminal.Peek().Append("'");
            if (!expression.IsCaseSensitive)
            {
                terminal.Peek().Append(@"\i");
            }
        }
        public override void Visit(NPEG.Terminals.CodePoint expression)
        {
            terminal.Peek().Append(expression.MatchText);
        }
        public override void Visit(NPEG.Terminals.DynamicBackReference expression)
        {
            terminal.Peek().Append(@"\k<");
            terminal.Peek().Append(expression.BackReferenceName);
            terminal.Peek().Append(">");
            if (!expression.IsCaseSensitive)
            {
                terminal.Peek().Append("[");
                terminal.Peek().Append(@"\i");
                terminal.Peek().Append("]");
            }
        }
        public override void Visit(NPEG.Terminals.RecursionCall expression)
        {
            // Terminal Reference Name
            terminal.Peek().Append(expression.FunctionName);
        }










        Dictionary<String, String> uniqueCapturedGroup = new Dictionary<string, string>();
        //  name, rule text
        public override void VisitEnter(NPEG.NonTerminals.CapturingGroup expression)
        {
            this.terminal.Push(new StringBuilder());
        }
        public override void VisitExecute(NPEG.NonTerminals.CapturingGroup expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.CapturingGroup expression)
        {
            String rule = this.terminal.Pop().ToString();

            StringBuilder nodeText = new StringBuilder();
            nodeText.Append("(?<");
            nodeText.Append(expression.Name);
            if (expression.ReplacementNode != null || expression.DoReplaceBySingleChildNode)
            {
                nodeText.Append("[");

                if (expression.DoReplaceBySingleChildNode)
                    nodeText.Append("\rsc");

                if (expression.ReplacementNode != null)
                {
                    nodeText.Append("\rn=[");
                    nodeText.Append(expression.ReplacementNode.GetType().FullName + ", " + Regex.Replace(expression.ReplacementNode.GetType().Module.Name, @"\.dll", "") );
                    nodeText.Append("]");
                }

                nodeText.Append("]");
            }
            nodeText.Append(">");


            if (this.uniqueCapturedGroup.ContainsKey(expression.Name))
            {
                if (this.uniqueCapturedGroup[expression.Name] == rule)
                {
                    this.terminal.Peek().Append(expression.Name);
                }
                else
                {
                    //same name but different rule so write inline
                    nodeText.Append(rule);
                    nodeText.Append(")");

                    this.terminal.Peek().Append(nodeText.ToString());
                }
            }
            else
            {
                if (this.uniqueCapturedGroup.ContainsValue(rule))
                {
                    // different name same rule
                    String name = this.uniqueCapturedGroup.Where(kvp => kvp.Value == rule).Select(kvp => kvp.Key).First();
                    this.terminal.Peek().Append(name);
                }
                else
                {
                    nodeText.Append("): ");
                    nodeText.Append(rule);
                    nodeText.Append(";");
                    statements.Add(nodeText.ToString());
                    this.uniqueCapturedGroup.Add(expression.Name, rule);

                    if (this.terminal.Count > 0)
                        this.terminal.Peek().Append(expression.Name);
                }
            }
        }


        Dictionary<String, String> uniqueRecursionBlocks = new Dictionary<string, string>();
            //rule, name
        public override void VisitEnter(NPEG.NonTerminals.RecursionCreate expression)
        {
            this.terminal.Push(new StringBuilder());
        }
        public override void VisitExecute(NPEG.NonTerminals.RecursionCreate expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.RecursionCreate expression)
        {
            String rule = this.terminal.Pop().ToString();
            if (!this.uniqueRecursionBlocks.ContainsKey(rule))
            {
                this.uniqueRecursionBlocks.Add(rule, expression.FunctionName);

                StringBuilder nodeText = new StringBuilder();
                nodeText.AppendFormat("{0}: {1};", expression.FunctionName, rule);
                statements.Add(nodeText.ToString());
            }

            if (this.terminal.Count > 0)
                this.terminal.Peek().Append(this.uniqueRecursionBlocks[rule]);
        }















        public override void VisitEnter(NPEG.NonTerminals.AndPredicate expression)
        {
            terminal.Peek().Append("&(");
        }
        public override void VisitExecute(NPEG.NonTerminals.AndPredicate expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.AndPredicate expression)
        {
            terminal.Peek().Append(") ");
        }



        public override void VisitEnter(NPEG.NonTerminals.NotPredicate expression)
        {
            terminal.Peek().Append("!(");
        }
        public override void VisitExecute(NPEG.NonTerminals.NotPredicate expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.NotPredicate expression)
        {
            terminal.Peek().Append(")");
        }














        public override void VisitEnter(NPEG.NonTerminals.OneOrMore expression)
        {
            terminal.Peek().Append("(");
        }
        public override void VisitExecute(NPEG.NonTerminals.OneOrMore expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.OneOrMore expression)
        {
            terminal.Peek().Append(")+");
        }



        public override void VisitEnter(NPEG.NonTerminals.Optional expression)
        {
            terminal.Peek().Append("(");
        }
        public override void VisitExecute(NPEG.NonTerminals.Optional expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.Optional expression)
        {
            terminal.Peek().Append(")?");
        }



        public override void VisitEnter(NPEG.NonTerminals.ZeroOrMore expression)
        {
            terminal.Peek().Append("(");
        }
        public override void VisitExecute(NPEG.NonTerminals.ZeroOrMore expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.ZeroOrMore expression)
        {
            terminal.Peek().Append(")*");
        }



        public override void VisitEnter(NPEG.NonTerminals.LimitingRepetition expression)
        {
            terminal.Peek().Append("(");
        }
        public override void VisitExecute(NPEG.NonTerminals.LimitingRepetition expression)
        {
        }
        public override void VisitLeave(NPEG.NonTerminals.LimitingRepetition expression)
        {
            terminal.Peek().Append("){");

            if (expression.Max == expression.Min)
            {
                if (expression.Min == null)
                    throw new ArgumentException("Min and Max should not be null.");

                // exact count
                terminal.Peek().Append(expression.Max.ToString());
            }
            else if (expression.Max == null)
            {
                // only min limit
                terminal.Peek().Append(expression.Min.ToString());
                terminal.Peek().Append(",");
            }
            else if (expression.Min == null)
            {
                // only max limit
                terminal.Peek().Append(",");
                terminal.Peek().Append(expression.Max.ToString());
            }
            else
            {
                // both min and max limit set
                terminal.Peek().Append(expression.Min.ToString());
                terminal.Peek().Append(",");
                terminal.Peek().Append(expression.Max.ToString());
            }

            terminal.Peek().Append(@"}");
        }










        #warning create nodes of repeated rules.. place this in prioritychoice / sequence
        Dictionary<String, String> uniqueBranches = new Dictionary<string, string>();
        // value of node, name
        Int32 branchcount = 0;
        
        public override void VisitEnter(NPEG.NonTerminals.PrioritizedChoice expression)
        {
            //this.terminal.Push(new StringBuilder());
            terminal.Peek().Append("(");
        }
        public override void VisitExecute(NPEG.NonTerminals.PrioritizedChoice expression)
        {
            terminal.Peek().Append(" / ");
        }
        public override void VisitLeave(NPEG.NonTerminals.PrioritizedChoice expression)
        {
            terminal.Peek().Append(")");

            //String input = terminal.Pop().ToString();
            //if (!this.uniqueBranches.ContainsKey(input))
            //{
            //    String nodename = "node" + branchcount++;
            //    this.uniqueBranches.Add(input, nodename);
            //    this.statements.Add( String.Format("{0}: {1};", nodename, input) );
            //}

            //// remember last node is always a captured group so peek should not throw exceptions
            //// insert terminal name
            //terminal.Peek().Append(this.uniqueBranches[input]);
        }




        public override void VisitEnter(NPEG.NonTerminals.Sequence expression)
        {
            //this.terminal.Push(new StringBuilder());
        }
        public override void VisitExecute(NPEG.NonTerminals.Sequence expression)
        {
            terminal.Peek().Append(" ");
        }
        public override void VisitLeave(NPEG.NonTerminals.Sequence expression)
        {
            //String input = terminal.Pop().ToString();
            //if (!this.uniqueBranches.ContainsKey(input))
            //{
            //    String nodename = "node" + branchcount++;
            //    this.uniqueBranches.Add(input, nodename);
            //    this.statements.Add(String.Format("{0}: {1};", nodename, input));
            //}

            //// remember last node is always a captured group so peek should not throw exceptions
            //// insert terminal name
            //terminal.Peek().Append(this.uniqueBranches[input]);
        }





        public String GrammarOutput
        {
            get {
                StringBuilder grammar = new StringBuilder();
                var q = this.statements.Distinct().ToArray();
                foreach(String s in q)
                {
                    grammar.Append(s + Environment.NewLine);
                }

                return grammar.ToString(); 
            
            }
        }

    }
}
