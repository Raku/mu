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
    [DebuggerDisplay("AstNode: {Token.Value}, Children {Children.Count}")]
    public class AstNode 
    {
        public AstNode()
        {
            this.Children = new List<AstNode>();
        }

        public AstNode Parent
        {
            get;
            set;
        }

        public List<AstNode> Children
        {
            get;
            set;
        }

        public TokenMatch Token
        {
            get;
            set;
        }


        public void Accept(IAstNodeReplacement visitor)
        {
            visitor.VisitEnter(this);

            Boolean isFirstTime = true;
            foreach (AstNode node in this.Children)
            {
                if (!isFirstTime)
                {
                    visitor.VisitExecute(this);
                }

                isFirstTime = false;
                node.Accept(visitor);
            }

            visitor.VisitLeave(this);
        }
    }
}
