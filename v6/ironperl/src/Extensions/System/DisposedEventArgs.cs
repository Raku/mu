using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// Represents arguments for the IDisposable.Disposing event
    /// </summary>
    [Serializable]
    public class DisposedEventArgs : EventArgs
    {
        private readonly bool m_disposing;

        /// <summary>
        /// Create instance of the DisposedEventArgs class
        /// </summary>
        /// <param name="disposing"></param>
        public DisposedEventArgs(bool disposing)
        {
            m_disposing = disposing;
        }

        /// <summary>
        /// If disposing equals true, the method has been called directly
        /// or indirectly by a user's code. Managed and unmanaged resources
        /// can be disposed.
        /// If disposing equals false, the method has been called by the
        /// runtime from inside the finalizer and you should not reference
        /// other objects. Only unmanaged resources can be disposed.
        /// </summary>
        public bool Disposing
        {
            get { return m_disposing; }
        }
    }
}
