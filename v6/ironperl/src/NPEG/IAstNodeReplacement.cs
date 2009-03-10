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
    // visitor
    abstract public class IAstNodeReplacement : AstNode, IDisposable
    {
        abstract public void VisitEnter(AstNode node);
        abstract public void VisitExecute(AstNode node);
        abstract public void VisitLeave(AstNode node);

        #region IDisposable Members

        abstract public void Dispose();

        #endregion
    }
}
