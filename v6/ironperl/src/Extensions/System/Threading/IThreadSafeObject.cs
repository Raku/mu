using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Threading
{
    /// <summary>
    /// Defines a basic functionality for all thread-safe objects
    /// </summary>
    public interface IThreadSafeObject
    {
        /// <summary>
        /// Indicates that the current object is locked by another thread
        /// </summary>
        bool Locked { get; }

        /// <summary>
        /// Locks specified object on the caller thread
        /// </summary>
        /// <param name="wait">If value is true then the caller thread is blocked until
        /// object is not released</param>
        /// <returns>true, if the caller thread can locks this object, otherwise false</returns>
        bool Lock(bool wait);

        /// <summary>
        /// Release the current object
        /// </summary>
        /// <returns></returns>
        bool Release();

        /// <summary>
        /// Get synchronization root
        /// </summary>
        object SyncRoot { get; }
    }
}
