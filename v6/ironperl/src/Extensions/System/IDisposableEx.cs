using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// Represents an extended IDispose interface
    /// </summary>
    public interface IDisposableEx : IDisposable
    {
        /// <summary>
        /// Occurs when the Dispose method is called 
        /// or when the object is finalized and collected by the garbage collector
        /// </summary>
        event EventHandler Disposed;
    }
}
