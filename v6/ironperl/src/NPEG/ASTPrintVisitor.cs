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

namespace NPEG
{
    public class ASTPrintVisitor : IAstNodeReplacement
    {
        public ASTPrintVisitor() 
        {
            System.Diagnostics.Debug.WriteLine("");
            System.Diagnostics.Debug.WriteLine("");
            System.Diagnostics.Debug.WriteLine("START - ASTPrintVisitor.cs");
        }

        public override void VisitEnter(AstNode node)
        {
            System.Diagnostics.Debug.WriteLine("VisitEnter: " + node.Token.Name + " " + ((node.Children.Count == 0) ? node.Token.Value : "") );
            System.Diagnostics.Debug.Indent();
        }

        public override void VisitExecute(AstNode node)
        {
            System.Diagnostics.Debug.WriteLine("VisitExecute: " + node.Token.Name );
        }

        public override void VisitLeave(AstNode node)
        {
            System.Diagnostics.Debug.Unindent();
            System.Diagnostics.Debug.WriteLine("VisitLeave: " + node.Token.Name );
        }



        public override void Dispose()
        {
            System.Diagnostics.Debug.WriteLine("END - ASTPrintVisitor.cs");
            System.Diagnostics.Debug.WriteLine("");
            System.Diagnostics.Debug.WriteLine("");
        }
    }
}
