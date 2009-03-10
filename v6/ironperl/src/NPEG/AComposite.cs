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

using NPEG.NonTerminals;

namespace NPEG
{
    abstract public class AComposite : AExpression
    {

        #region Predicates
        public AndPredicate And()
        {
            return new AndPredicate(this);
        }

        public NotPredicate Not()
        {
            return new NotPredicate(this);
        }
        #endregion


        #region binary
        public PrioritizedChoice Or(AExpression other)
        {
            return new PrioritizedChoice(this, other);
        }

        public Sequence Sequence(AExpression other)
        {
            return new Sequence(this, other);
        }
        #endregion


        #region unary suffix
        public Optional Optional()
        {
            return new Optional(this);
        }

        public OneOrMore Plus()
        {
            return new OneOrMore(this);
        }

        public ZeroOrMore Star()
        {
            return new ZeroOrMore(this);
        }

        public LimitingRepetition Limit(Int32? min, Int32? max)
        {
            return new LimitingRepetition(this) { Min = min, Max = max };
        }
        #endregion


        public CapturingGroup Capture(String name)
        {
            return new CapturingGroup(name, this);
        }





        abstract public IEnumerable<AExpression> GetChildren();

        public override void Accept(ICompositeGrammarVisitor visitor)
        {
            visitor.VisitEnter(this);

            int i = 0;
            foreach (AExpression expression in this.GetChildren())
            {
                if (i++ != 0)
                {
                    visitor.VisitExecute(this);
                }

                expression.Accept(visitor);
            }

            visitor.VisitLeave(this);
        }
    }
}
