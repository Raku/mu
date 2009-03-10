using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents state-graph node.
    /// </summary>
    /// <typeparam name="T">Type of the state identifier</typeparam>
    public sealed class StateGraphNode<T>: IDiscreteState
    {
        private readonly T m_tag;
        private StateGraphNode<T>[] m_states;

        /// <summary>
        /// Create state-graph node with specified
        /// Tag value
        /// </summary>
        /// <param name="stateId">State identifier.</param>
        internal StateGraphNode(T stateId)
        {
            m_tag = stateId;
            AvailableStates = null;
        }

        /// <summary>
        /// Get state identifier.
        /// </summary>
        public T StateId
        {
            get { return m_tag; }
        }

        /// <summary>
        /// Gets or sets the object that contains data about the graph node.
        /// </summary>
        public object Tag
        {
            get;
            set;
        }

        /// <summary>
        /// Get or next available states.
        /// </summary>
        /// <remarks>If there is no available states then
        /// property returns empty array.</remarks>
        internal StateGraphNode<T>[] AvailableStates
        {
            get { return m_states; }
            set
            {
                m_states = value.IsNull() ? new StateGraphNode<T>[0] : value;
            }
        }

        #region IDiscreteState Members

        ObjectStateType IDiscreteState.StateKind
        {
            get
            {
                return AvailableStates.Length.IsZero() ?
                    ObjectStateType.Final :
                    ObjectStateType.Transient;
            }
        }

        #endregion
    }
}
