using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents object that has a infinite number of
    /// states
    /// </summary>
    /// <typeparam name="T">Type of the state identifier</typeparam>
	public interface IStateSupported<T>
	{
        /// <summary>
        /// Get or set state identifier of the current object
        /// </summary>
        T CurrentState { get; set; }

        /// <summary>
        /// Returns value indicating that the specified state identifier
        /// is a valid identifier for the current controller
        /// </summary>
        /// <param name="stateId">Object state identifier</param>
        /// <returns>Value indicating that the specified state identifier
        /// is a valid identifier for the current controller</returns>
        bool IsValidState(T stateId);

        /// <summary>
        /// Release all resources associated with specified object state
        /// </summary>
        /// <param name="stateId"></param>
        void CleanupState(T stateId);

        /// <summary>
        /// Occurs, when object state is changed.
        /// </summary>
        event EventHandler StateChanged;
	}
}
