using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.Serialization;

namespace System
{
    using ExceptionManager = global::System.Extensions.ExceptionManager;

    /// <summary>
    /// Represents switch case
    /// </summary>
    [Serializable]
    public sealed class SwitchCase<T>: Serializable
    {
        private readonly T m_value;
        private readonly Action<T> m_action;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private SwitchCase()
        {
        }

        /// <summary>
        /// Create switch-case block
        /// </summary>
        /// <param name="value"></param>
        /// <param name="action"></param>
        public SwitchCase(T value, Action<T> action)
        {
            ExceptionManager.CheckOnNull(action, "action");
            m_action = action;
            m_value = value;
        }

        internal T Value
        {
            get { return m_value; }
        }

        /// <summary>
        /// Fires specified action
        /// </summary>
        public void FireAction()
        {
            m_action(Value);
        }
    }
}
