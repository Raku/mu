using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Runtime.States
{
    /// <summary>
    /// Represents object state controller.
    /// </summary>
    /// <typeparam name="T">Type of the state identifier.</typeparam>
    public sealed class StateController<T> : IDiscreteObject<StateGraphNode<T>>
    {
        private StateGraphNode<T> m_currentState;

        private StateController(StateGraphNode<T> initialGraph)
        {
            m_currentState = initialGraph;
        }

        #region IDiscreteObject<StateGraphNode<T>> Members

        /// <summary>
        /// Get available states.
        /// </summary>
        public StateGraphNode<T>[] AvailableStates
        {
            get { return CurrentState.AvailableStates; }
        }

        /// <summary>
        /// Get identifiers of the available states.
        /// </summary>
        /// <returns>Identifier of the available states.</returns>
        public T[] GetAvailableStateIds()
        {
            var result = from state in AvailableStates
                         select state.StateId;
            return result.ToArray();
        }

        #endregion

        #region IStateSupported<StateGraphNode<T>> Members

        /// <summary>
        /// Get or set state of the current object.
        /// </summary>
        /// <exception cref="System.InvalidOperationException">Unexpected object state.
        /// Use StateController.IsValidState to validate new object state.</exception>
        public StateGraphNode<T> CurrentState
        {
            get
            {
                return m_currentState;
            }
            set
            {
                if (IsValidState(value))
                    m_currentState = value;
                else
                    throw new InvalidOperationException();
                OnStateChanged();
            }
        }

        /// <summary>
        /// Determines whether specified state is supported by the current object
        /// in the current state.
        /// </summary>
        /// <param name="stateId">New state identifier.</param>
        /// <returns>True, if specified state is valid; otherwise, false.</returns>
        public bool IsValidState(StateGraphNode<T> stateId)
        {
            if (stateId.IsNull()) return false;
            foreach (var nextState in AvailableStates)
                if (nextState.Equals(stateId)) return true;
            return false;
        }

        void IStateSupported<StateGraphNode<T>>.CleanupState(StateGraphNode<T> stateId)
        {

        }

        /// <summary>
        /// Occurs when the current object state is changed.
        /// </summary>
        public event EventHandler StateChanged;

        #endregion

        private void OnStateChanged()
        {
            if (StateChanged.IsNotNull())
                StateChanged(this, new EventArgs());
        }

        /// <summary>
        /// Create initial state-graph node. 
        /// </summary>
        /// <param name="stateId">State identifier.</param>
        /// <returns>Initial state-graph node.</returns>
        public static StateGraphNode<T> CreateInitialNode(T stateId)
        {
            return new StateGraphNode<T>(stateId);
        }

        /// <summary>
        /// Create state controller from specified initial state.
        /// </summary>
        /// <param name="initialGraph">Initial state for controller.</param>
        /// <returns>State controller.</returns>
        public static StateController<T> CreateController(StateGraphNode<T> initialGraph)
        {
            return new StateController<T>(initialGraph);
        }

        /// <summary>
        /// Set child states for specified state node.
        /// </summary>
        /// <param name="root"></param>
        /// <param name="childNodes"></param>
        public static void SetNodeStates(StateGraphNode<T> root, params StateGraphNode<T>[] childNodes)
        {
            root.AvailableStates = childNodes;
        }
    }
}
