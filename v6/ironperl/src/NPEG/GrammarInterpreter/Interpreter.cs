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

namespace NPEG.GrammarInterpreter
{
    public class Interpreter : IAstNodeReplacement
    {
        public Interpreter()
        {
        }

        public override void VisitEnter(AstNode node)
        {
        }
        public override void VisitExecute(AstNode node)
        {
        }
        public override void VisitLeave(AstNode node)
        {
        }
        public override void Dispose()
        {
        }


        Dictionary<String, AExpression> completedStatements = new Dictionary<string, AExpression>();
        List<String> wrapWithRecursionRule = new List<string>();
        
        public AExpression this[String terminalname]
        {
            get
            {
                if (this.completedStatements.ContainsKey(terminalname))
                {
                    // a non terminal requesting a terminal
                    return this.completedStatements[terminalname];
                }
                else
                {
                    Boolean requestingRecursion = false;
                    foreach (StatementAstNode node in this.Children)
                    {
                        if (node.NodeDefinition == terminalname)
                        {
                            requestingRecursion = true;
                        }
                    }

                    if (requestingRecursion)
                    {
                        // terminal wanting to recursively call a non terminal
                        // that will be later defined in the document
                       
                        if (!this.wrapWithRecursionRule.Contains(terminalname))
                            this.wrapWithRecursionRule.Add(terminalname);

                        return new Terminals.RecursionCall(terminalname);
                    }
                    else
                    { 
                        // oops requesting terminal nowhere defined in the document.
                        throw new ParseException("PEG Statement cannot be created.  Requesting '" + terminalname + "' terminal which is not defined in grammar rules.");
                    }
                }
            }        
        }


        AExpression expression;
        public AExpression Expression
        {
            get
            {
                if (expression == null)
                {
                    // Assumes Terminals are at the top of the file and 
                    // final root non terminal expression is at the bottom.
                    foreach (StatementAstNode node in this.Children)
                    {
                        if (this.wrapWithRecursionRule.Contains(node.NodeDefinition))
                            expression = new NonTerminals.RecursionCreate(node.NodeDefinition, node.Expression);
                        else
                            expression = node.Expression;

                        this.completedStatements.Add(node.NodeDefinition, expression);
                    }

                    if (expression != null)
                    {
                        if ( expression.GetType() == typeof(NonTerminals.RecursionCreate) )
                        {
                            if (((NonTerminals.RecursionCreate)expression).TypeContains != typeof(NonTerminals.CapturingGroup))
                            {
                                throw new ParseException("Root terminal requires it be of type capturing group.  Wrap last terminal in (?<RootName>).");
                            }
                        }
                        else if (expression.GetType() != typeof(NonTerminals.CapturingGroup))
                        {
                            throw new ParseException("Root terminal requires it be of type capturing group.  Wrap last terminal in (?<RootName>).");
                        }
                    }
                }

                // assumes last terminal is root
                return expression; 
            }
        }

    }
}
