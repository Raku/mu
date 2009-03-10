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
using System.Reflection;

using NPEG.NonTerminals;
using NPEG.Terminals;

namespace NPEG
{
    public abstract class ICompositeGrammarVisitor : IDisposable
    {
        public ICompositeGrammarVisitor()
        {
        }


        public void VisitEnter(AExpression expression)
        {
            // Use reflection to find and invoke the correct Visit method
            Type[] types = new Type[] { expression.GetType() };
            MethodInfo methodInfo = this.GetType().GetMethod("VisitEnter", types);
            if (methodInfo != null)
            {
                methodInfo.Invoke(this, new object[] { expression });
            }
            else
            {
                throw new Exception("Visitor does not implement the Visit method for the type: " + expression.GetType().ToString());
            }
        }

        public void VisitExecute(AExpression expression)
        {
            // Use reflection to find and invoke the correct Visit method
            Type[] types = new Type[] { expression.GetType() };
            MethodInfo methodInfo = this.GetType().GetMethod("VisitExecute", types);
            if (methodInfo != null)
            {
                methodInfo.Invoke(this, new object[] { expression });
            }
            else
            {
                throw new Exception("Visitor does not implement the Visit method for the type: " + expression.GetType().ToString());
            }
        }

        public void VisitLeave(AExpression expression)
        {
            // Use reflection to find and invoke the correct Visit method
            Type[] types = new Type[] { expression.GetType() };
            MethodInfo methodInfo = this.GetType().GetMethod("VisitLeave", types);
            if (methodInfo != null)
            {
                methodInfo.Invoke(this, new object[] { expression });
            }
            else
            {
                throw new Exception("Visitor does not implement the Visit method for the type: " + expression.GetType().ToString());
            }
        }


        public void Visit(AExpression expression)
        {
            // Use reflection to find and invoke the correct Visit method
            Type[] types = new Type[] { expression.GetType() };
            MethodInfo methodInfo = this.GetType().GetMethod("Visit", types);
            if (methodInfo != null)
            {
                methodInfo.Invoke(this, new object[] { expression });
            }
            else
            {
                throw new Exception("Visitor does not implement the Visit method for the type: " + expression.GetType().ToString());
            }
        }


        #region nonterminals
        abstract public void VisitEnter(AndPredicate expression);
        abstract public void VisitExecute(AndPredicate expression);
        abstract public void VisitLeave(AndPredicate expression);

        abstract public void VisitEnter(PrioritizedChoice expression);
        abstract public void VisitExecute(PrioritizedChoice expression);
        abstract public void VisitLeave(PrioritizedChoice expression);

        abstract public void VisitEnter(NotPredicate expression);
        abstract public void VisitExecute(NotPredicate expression);
        abstract public void VisitLeave(NotPredicate expression);

        abstract public void VisitEnter(ZeroOrMore expression);
        abstract public void VisitExecute(ZeroOrMore expression);
        abstract public void VisitLeave(ZeroOrMore expression);

        abstract public void VisitEnter(OneOrMore expression);
        abstract public void VisitExecute(OneOrMore expression);
        abstract public void VisitLeave(OneOrMore expression);

        abstract public void VisitEnter(Optional expression);
        abstract public void VisitExecute(Optional expression);
        abstract public void VisitLeave(Optional expression);

        abstract public void VisitEnter(Sequence expression);
        abstract public void VisitExecute(Sequence expression);
        abstract public void VisitLeave(Sequence expression);

        abstract public void VisitEnter(CapturingGroup expression);
        abstract public void VisitExecute(CapturingGroup expression);
        abstract public void VisitLeave(CapturingGroup expression);

        abstract public void VisitEnter(RecursionCreate expression);
        abstract public void VisitExecute(RecursionCreate expression);
        abstract public void VisitLeave(RecursionCreate expression);

        abstract public void VisitEnter(LimitingRepetition expression);
        abstract public void VisitExecute(LimitingRepetition expression);
        abstract public void VisitLeave(LimitingRepetition expression);
        #endregion


        #region terminals
        abstract public void Visit(Literal expression);
        abstract public void Visit(CharacterClass expression);
        abstract public void Visit(AnyCharacter expression);
        abstract public void Visit(RecursionCall expression);
        abstract public void Visit(DynamicBackReference expression);
        abstract public void Visit(CodePoint expression);
        #endregion


        #region IDisposable Members

        abstract public void Dispose();

        #endregion
    }
}
