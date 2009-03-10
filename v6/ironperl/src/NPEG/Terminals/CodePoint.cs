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
    // #32 (decimal) // Match input against the specified unicode character
    // #x3A0 (hex)
    // #b111 (binary)
    public class CodePoint: AExpression
    {
        // used when input form incomplete boundaries
        // meaning empty slots are available (determine default slot value using ShiftIntoValue).
        public enum ShiftDirectionType
        {
            Right, Left
        }

        public CodePoint()
            : base()
        {
            this.ShiftDirection = ShiftDirectionType.Right;
            this.ShiftIntoValue = "0";
        }

        public override void Accept(ICompositeGrammarVisitor visitor)
        {
            visitor.Visit(this);
        }


        public String MatchText
        {
            get;
            set;
        }

        public ShiftDirectionType ShiftDirection
        {
            get;
            set;
        }
        public String ShiftIntoValue
        {
            get;
            set;
        }
    }
}
