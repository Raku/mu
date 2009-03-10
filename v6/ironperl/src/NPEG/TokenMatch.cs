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
using System.Diagnostics;

namespace NPEG
{
    [DebuggerDisplay("TokenMatch: {Name} = {Value}")]
    public class TokenMatch
    {
        public TokenMatch(String name, String value, Int32 start, Int32 end)
        {
            this.Name = name;
            this.Value = value;
            this.Start = start;
            this.End = end;        
        }

        public String Name
        {
            private set;
            get;
        }

        public String Value
        {
            private set;
            get;
        }

        public Int32 Start
        {
            private set;
            get;
        }

        public Int32 End
        {
            private set;
            get;
        }
    }
}
