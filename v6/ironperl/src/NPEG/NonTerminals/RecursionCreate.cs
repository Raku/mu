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
    public class RecursionCreate : AComposite
    {      
        private AExpression exp;

        public RecursionCreate(String unique, AExpression exp)
        {
            this.FunctionName = unique;
            this.exp = exp;
        }

        public String FunctionName
        {
            get;
            set;
        }

        public Type TypeContains
        {
            get { return this.exp.GetType(); }
        }

        public override IEnumerable<AExpression> GetChildren()
        {
            yield return exp;
        }
    }
}
