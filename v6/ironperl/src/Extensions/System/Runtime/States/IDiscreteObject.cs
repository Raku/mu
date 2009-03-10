using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents object that has a finite number of states
    /// </summary>
    public interface IDiscreteObject<T> : IStateSupported<T>
        where T : IDiscreteState
    {
        /// <summary>
        /// Get all states in which transition from the current state
        /// is possible
        /// </summary>
        T[] AvailableStates { get; }
    }
}
