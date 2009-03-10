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
    public class CapturingGroup : AComposite
    {
        private AExpression Exp;

        public CapturingGroup(String uniquename, AExpression exp)
        {
            this.Exp = exp;
            this.Name = uniquename;
            this.DoReplaceBySingleChildNode = false;
        }

        public CapturingGroup(String uniquename, IAstNodeReplacement onmatchcreate, AExpression exp)
        {
            this.Exp = exp;
            this.Name = uniquename;
            this.DoReplaceBySingleChildNode = false;
            this.ReplacementNode = onmatchcreate;
        }

        public String Name
        {
            get;
            set;
        }

        public Boolean DoReplaceBySingleChildNode 
        {
            get;
            set;
        }

        public IAstNodeReplacement ReplacementNode
        {
            get;
            set;
        }

        public override IEnumerable<AExpression> GetChildren()
        {
            yield return Exp;
        }
    }
}
