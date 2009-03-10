using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System
{
    /// <summary>
    /// Represents a switch block
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public sealed class SwitchBlock<T> : ICollection<SwitchCase<T>>
    {
        private readonly Dictionary<T, SwitchCase<T>> m_cases;
        private Action<T> m_default;

        /// <summary>
        /// Create switch-block with predefined case-handlers
        /// </summary>
        /// <param name="cases"></param>
        public SwitchBlock(params SwitchCase<T>[] cases)
        {
            m_cases = new Dictionary<T, SwitchCase<T>>();
            if (cases.IsNotNull())
                foreach (var _case in cases)
                    Add(_case);
        }

        /// <summary>
        /// Execute switch-block
        /// </summary>
        /// <param name="value">Inner value, which must be proceed by the block</param>
        public void Execute(T value)
        {
            if (m_cases.ContainsKey(value).IsFalse())
            {
                if (Default.IsNotNull())
                    Default(value);
                return;
            }
            var block = m_cases[value];
            block.FireAction();
        }

        /// <summary>
        /// Represents a default code block
        /// </summary>
        public Action<T> Default
        {
            get { return m_default; }
        }

        /// <summary>
        /// Add case-handler for specified value
        /// </summary>
        /// <param name="value">Value fr the case-expression</param>
        /// <param name="action">Trigger for the specified value</param>
        public void Add(T value, Action<T> action)
        {
            Add(new SwitchCase<T>(value, action));
        }

        /// <summary>
        /// Add a default handler
        /// </summary>
        /// <param name="defaultAction">Default handler, which will be invoked if
        /// inner value doesn't match to any case-block</param>
        public void Add(Action<T> defaultAction)
        {
            m_default = defaultAction;
        }

        private void Add(SwitchCase<T> item)
        {
            m_cases.Add(item.Value, item);
        }

        #region ICollection<SwitchCase<T>> Members

        void ICollection<SwitchCase<T>>.Add(SwitchCase<T> item)
        {
            Add(item);
        }

        /// <summary>
        /// Delete all handlers
        /// </summary>
        public void Clear()
        {
            m_cases.Clear();
            m_default = null;
        }

        /// <summary>
        /// Returns value indicating that the specified case-handler
        /// is present in the current switch-block
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public bool Contains(SwitchCase<T> item)
        {
            return m_cases.ContainsKey(item.Value);
        }

        /// <summary>
        /// Copy a portion of case-handlers to the specified array
        /// </summary>
        /// <param name="array"></param>
        /// <param name="arrayIndex"></param>
        public void CopyTo(SwitchCase<T>[] array, int arrayIndex)
        {
            m_cases.Values.CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Get count of handlers
        /// </summary>
        public int Count
        {
            get { return m_cases.Count; }
        }

        bool ICollection<SwitchCase<T>>.IsReadOnly
        {
            get { return false; }
        }

        /// <summary>
        /// Remove specified case-handler from switch-block
        /// </summary>
        /// <param name="item">case-handler to be deleted</param>
        /// <returns></returns>
        public bool Remove(SwitchCase<T> item)
        {
            return m_cases.Remove(item.Value);
        }

        #endregion

        #region IEnumerable<SwitchCase<T>> Members

        /// <summary>
        /// Get all case-handlers
        /// </summary>
        /// <returns></returns>
        public IEnumerator<SwitchCase<T>> GetEnumerator()
        {
            var enumerable = from t in m_cases
                             select t.Value;
            return enumerable.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            throw new NotImplementedException();
        }

        #endregion
    }
}
