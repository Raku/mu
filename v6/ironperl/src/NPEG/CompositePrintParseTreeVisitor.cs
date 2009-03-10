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
    public class CompositePrintParseTreeVisitor : ICompositeGrammarVisitor
    {
        StringBuilder tree = new StringBuilder();
        public CompositePrintParseTreeVisitor()
        {
            System.Diagnostics.Debug.WriteLine("");
            System.Diagnostics.Debug.WriteLine("");
            System.Diagnostics.Debug.WriteLine("Start - CompositePrintParseTreeVisitor");
        }

        public override void Dispose()
        {
            System.Diagnostics.Debug.WriteLine("End   - CompositePrintParseTreeVisitor");
            System.Diagnostics.Debug.WriteLine("");
            System.Diagnostics.Debug.WriteLine("");
        }

        public String ParseTree
        {
            get { return this.tree.ToString(); }
        }

        public override void Visit(NPEG.Terminals.AnyCharacter expression)
        {
            System.Diagnostics.Debug.WriteLine("Visit: AnyCharacter");
        }
        public override void Visit(NPEG.Terminals.CharacterClass expression)
        {
            System.Diagnostics.Debug.WriteLine("Visit: CharacterClass; " + expression.ClassExpression);
        }
        public override void Visit(NPEG.Terminals.Literal expression)
        {
            System.Diagnostics.Debug.WriteLine("Visit: Literal; " + expression.MatchText);
        }
        public override void Visit(NPEG.Terminals.RecursionCall expression)
        {
            System.Diagnostics.Debug.WriteLine("Visit: RecursionCall");
        }
        public override void Visit(NPEG.Terminals.DynamicBackReference expression)
        {
            System.Diagnostics.Debug.WriteLine("Visit: DynamicBackReference");
        }
        public override void Visit(NPEG.Terminals.CodePoint expression)
        {
            System.Diagnostics.Debug.WriteLine("Visit: CodePoint");
        }












        public override void VisitEnter(NPEG.NonTerminals.CapturingGroup expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: CapturingGroup");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.CapturingGroup expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: CapturingGroup");
        }
        public override void VisitLeave(NPEG.NonTerminals.CapturingGroup expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: CapturingGroup");
        }


        public override void VisitEnter(NPEG.NonTerminals.RecursionCreate expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: RecursionCreate");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.RecursionCreate expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: RecursionCreate");
        }
        public override void VisitLeave(NPEG.NonTerminals.RecursionCreate expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: RecursionCreate");
        }















        public override void VisitEnter(NPEG.NonTerminals.AndPredicate expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: AndPredicate");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.AndPredicate expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: AndPredicate");
        }
        public override void VisitLeave(NPEG.NonTerminals.AndPredicate expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: AndPredicate");
        }



        public override void VisitEnter(NPEG.NonTerminals.NotPredicate expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: NotPredicate");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.NotPredicate expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: NotPredicate");
        }
        public override void VisitLeave(NPEG.NonTerminals.NotPredicate expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: NotPredicate");
        }














        public override void VisitEnter(NPEG.NonTerminals.OneOrMore expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: OneOrMore");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.OneOrMore expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: OneOrMore");
        }
        public override void VisitLeave(NPEG.NonTerminals.OneOrMore expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: OneOrMore");
        }



        public override void VisitEnter(NPEG.NonTerminals.Optional expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: Optional");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.Optional expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: Optional");
        }
        public override void VisitLeave(NPEG.NonTerminals.Optional expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: Optional");
        }



        public override void VisitEnter(NPEG.NonTerminals.ZeroOrMore expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: ZeroOrMore");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.ZeroOrMore expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: ZeroOrMore");
        }
        public override void VisitLeave(NPEG.NonTerminals.ZeroOrMore expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: ZeroOrMore");
        }



        public override void VisitEnter(NPEG.NonTerminals.LimitingRepetition expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: LimitingRepetition");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.LimitingRepetition expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: LimitingRepetition");
        }
        public override void VisitLeave(NPEG.NonTerminals.LimitingRepetition expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: LimitingRepetition");
        }















        public override void VisitEnter(NPEG.NonTerminals.PrioritizedChoice expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: PrioritizedChoice");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.PrioritizedChoice expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: PrioritizedChoice");
        }
        public override void VisitLeave(NPEG.NonTerminals.PrioritizedChoice expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: PrioritizedChoice");
        }




        public override void VisitEnter(NPEG.NonTerminals.Sequence expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: Sequence");
            System.Diagnostics.Debug.Indent();
        }
        public override void VisitExecute(NPEG.NonTerminals.Sequence expression)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: Sequence");
        }
        public override void VisitLeave(NPEG.NonTerminals.Sequence expression)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: Sequence");
        }
    }
}
