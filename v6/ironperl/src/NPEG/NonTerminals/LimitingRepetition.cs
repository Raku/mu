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
    //e{min,max}                // Match input at least min times but not more than max times against e. 
    //e{,max}                   // Match input at zero or more times but not more than max times against e. 
    //e{min,}                   // Match input at least min times against e. (no limit on max)
    //e{exactcount}             // Match input a total of exactcount agaist e.
    public class LimitingRepetition : AComposite
    {        
        private AExpression Exp;
        public LimitingRepetition(AExpression exp)
        {
            this.Exp = exp;
        }

        public override IEnumerable<AExpression> GetChildren()
        {
            yield return Exp;
        }

        public Int32? Min
        {
            get;
            set;
        }

        public Int32? Max
        {
            get;
            set;
        }
    }
}
