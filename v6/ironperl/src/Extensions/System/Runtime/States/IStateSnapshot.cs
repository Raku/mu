using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents log entry for Transaction Log
    /// </summary>
    public interface IStateSnapshot: IEquatable<IStateSnapshot>
    {
        /// <summary>
        /// Get date and time of this
        /// </summary>
        DateTime TimeStamp { get; }

        /// <summary>
        /// Get object state at the snapshot creation moment
        /// </summary>
        object State { get; }

        /// <summary>
        /// Indicates that the current object state is linked state.
        /// It means that the current state snapshot depends on previous states
        /// and take effect to the next state
        /// </summary>
        bool Linked { get; }
    }
}
