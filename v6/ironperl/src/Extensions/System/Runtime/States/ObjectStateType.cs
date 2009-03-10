using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents state kind of the discrete state machine.
    /// </summary>
    [Serializable]
    public enum ObjectStateType : byte
    {
        /// <summary>
        /// State cannot be determined as final or transient.
        /// </summary>
        Unstable,

        /// <summary>
        /// Indicates the transient state.
        /// </summary>
        Transient,

        /// <summary>
        /// Indicates the final state for the object.
        /// </summary>
        Final
    }
}
