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

namespace NPEG.NonTerminals
{
    public class Sequence : AComposite
    {        
        private AExpression left;
        private AExpression right;

        public Sequence(AExpression left, AExpression right)
        {
            this.left = left;
            this.right = right;
        }

        public override IEnumerable<AExpression> GetChildren()
        {
            yield return left;
            yield return right;
        }
    }
}
