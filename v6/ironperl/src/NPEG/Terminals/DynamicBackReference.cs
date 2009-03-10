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

namespace NPEG.Terminals
{
    // PEG by nature already support static back referencing.
    // this allows a user to match (, {, /*, and appropriate closing character.
    // however if a user wanted to parse xml DynamicBackReferencing is needed as what the parser must match
    // is not known ahead of time.
    public class DynamicBackReference : AExpression
    {
        public DynamicBackReference()
            : base()
        {
            this.IsCaseSensitive = true;
        }

        public override void Accept(ICompositeGrammarVisitor visitor)
        {
            visitor.Visit(this);
        }

        public String BackReferenceName
        {
            get;
            set;
        }

        public Boolean IsCaseSensitive
        {
            get;
            set;
        }
    }
}
