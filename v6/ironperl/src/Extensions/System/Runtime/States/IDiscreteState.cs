using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents state for the discrete state machine.
    /// </summary>
    public interface IDiscreteState
    {
        /// <summary>
        /// Get type of the state.
        /// </summary>
        ObjectStateType StateKind { get; }
    }
}
