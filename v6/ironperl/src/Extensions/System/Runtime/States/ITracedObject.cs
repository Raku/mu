using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents a basic functionality for objects, which are supporting
    /// state logging
    /// </summary>
    public interface ILoggedObject<T>: IStateSupported<T>
    {
        /// <summary>
        /// Begin object state recording
        /// </summary>
        void BeginRecord();

        /// <summary>
        /// End object state recording
        /// </summary>
        /// <returns>Object changes, which are performed during object usage between
        /// BeginRecord() and EndRecord() calls</returns>
        IStateSnapshot[] EndRecord();
    }
}
