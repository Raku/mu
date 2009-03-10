using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// Represents type of the object cloning
    /// </summary>
    [Serializable]
    public enum ObjectCopy : byte
    {
        /// <summary>
        /// Performs default CLR cloning mechanism
        /// </summary>
        Default,

        /// <summary>
        /// Performs memberwise cloning
        /// </summary>
        Shallow,
        
        /// <summary>
        /// Performs in-deph cloning. Each object in the object graph must be copied.
        /// </summary>
        Deep
    }
}
