using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections.Generic.Extensions;
using System.Extensions;

namespace System.Collections.Processing
{
    using ExceptionManager = System.Extensions.ExceptionManager;
    using Serializable = System.Runtime.Serialization.Serializable;

    /// <summary>
    /// Represents navigator for enumerations
    /// </summary>
    [Serializable]
    public sealed class SequenceNavigator<T>: Serializable, INavigationContext<T>
    {
        private readonly T[] m_cache;
        private int m_marker;

        /// <summary>
        /// Create navigator for specified enumeration
        /// </summary>
        /// <param name="enumerable"></param>
        public SequenceNavigator(IEnumerable<T> enumerable)
        {
            ExceptionManager.CheckOnNull(enumerable, "enumerable");
            m_cache = enumerable.ToArray();
            m_marker = 0;
        }

        private void ValidateMarker(object marker)
        {
            if (IsValidMarker(marker).IsFalse())
                ExceptionManager.Throw<InvalidOperationException>(System.Properties.Resources.Sting_InvalidMarker);
        }

        #region INavigationContext<T> Members

        /// <summary>
        /// Get value, which position is defined by marker
        /// </summary>
        /// <param name="marker">Marker, which determines a value position</param>
        /// <returns></returns>
        public T GetValueAtMarker(object marker)
        {
            ValidateMarker(marker);
            return m_cache[m_marker];
        }
        /// <summary>
        /// Get or set the current position of navigation
        /// </summary>
        public object CurrentPosition
        {
            get
            {
                return m_marker;
            }
            set
            {
                ValidateMarker(value);
                if (m_cache.Length.IsZero()) return;
                m_marker = value.UnsafeCast<int>();
            }
        }

        /// <summary>
        /// Get marked, which determines the next value in the sequence.
        /// </summary>
        public object Next
        {
            get 
            {
                var nextMarker = m_marker + 1;
                return nextMarker >= m_cache.Length ? null : nextMarker as object;
            }
        }

        /// <summary>
        /// Get marker, which determines the previous value in the sequence.
        /// </summary>
        public object Previous
        {
            get 
            {
                var prevMarker = m_marker - 1;
                return prevMarker < 0 ? null : prevMarker as object;
            }
        }

        /// <summary>
        /// Returns value indicating that specified marker is valid
        /// for the current navigator.
        /// Expected integer value
        /// </summary>
        /// <param name="marker">Marker to be checked</param>
        /// <returns></returns>
        public bool IsValidMarker(object marker)
        {
            return marker.InstanceOf<int>();
        }

        #endregion
    }
}

