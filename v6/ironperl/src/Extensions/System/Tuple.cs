/*  msw 2009-03-04 - using Tuple from Lokad.Shared instead...
using System.Diagnostics;
using System.Extensions;
using System.Runtime.Serialization;
namespace System
{
    /// <summary>
    /// Represents a double-typed value
    /// </summary>
    /// <typeparam name="TBase">A base class for other types</typeparam>
    /// <typeparam name="TFirst">A first type of the value</typeparam>
    /// <typeparam name="TSecond">A second type of the value</typeparam>
    [DebuggerDisplay("Value = {m_value}")]
    [Serializable]
    public struct Tuple<TFirst, TSecond> : IEquatable<Tuple<TFirst, TSecond>>
    {
        private readonly object m_value;

        /// <summary>
        /// Create tuple with specified value
        /// </summary>
        /// <param name="value">Tuple value</param>
        public Tuple(TFirst value)
        {
            m_value = value;
        }

        /// <summary>
        /// Create tuple with specified value
        /// </summary>
        /// <param name="value">Tuple value</param>
        public Tuple(TSecond value)
        {
            m_value = value;
        }

        /// <summary>
        /// Get one of the two possible values
        /// </summary>
        public TFirst First
        {
            get { return GetValue<TFirst>(); }
        }

        /// <summary>
        /// Get one of the two possible values
        /// </summary>
        public TSecond Second
        {
            get { return GetValue<TSecond>(); }
        }

        /// <summary>
        /// Get a tuple value
        /// </summary>
        /// <typeparam name="G">Requested type</typeparam>
        /// <returns>Unboxed tuple value</returns>
        public G GetValue<G>()
        {
            var typeInfo = typeof(G);
            if (typeInfo.Equals(typeof(TFirst)))
                return m_value.UnsafeCast<G>();
            if (typeInfo.Equals(typeof(TSecond)))
                return m_value.UnsafeCast<G>();
            var message = String.Format("Expected {0} or {1} type", typeof(TFirst).Name, typeof(TSecond).Name);
            ExceptionManager.Throw<ArgumentException>(message, "G");
            return default(G);
        }

        public static explicit operator TFirst(Tuple<TFirst, TSecond> tuple)
        {
            return tuple.First;
        }

        public static explicit operator TSecond(Tuple<TFirst, TSecond> tuple)
        {
            return tuple.Second;
        }

        public static implicit operator Tuple<TFirst, TSecond>(TFirst value)
        {
            return new Tuple<TFirst, TSecond>(value);
        }

        public static implicit operator Tuple<TFirst, TSecond>(TSecond value)
        {
            return new Tuple<TFirst, TSecond>(value);
        }

        #region IEquatable<Tuple<TBase,TFirst,TSecond>> Members

        /// <summary>
        /// Determines wether the current tuple is equal to another tuple
        /// </summary>
        /// <param name="other">The tuple to compare with the current tuple</param>
        /// <returns>true if the specified tuple 
        /// is equal to the current tuple; otherwise, false.</returns>
        public bool Equals(Tuple<TFirst, TSecond> other)
        {
            return m_value.Equals(other.m_value);
        }

        #endregion

        /// <summary>
        /// Determines wether the current tuple is equal to another tuple
        /// </summary>
        /// <param name="other">The tuple to compare with the current tuple</param>
        /// <returns>true if the specified tuple 
        /// is equal to the current tuple; otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            return obj.InstanceOf<Tuple<TFirst, TSecond>>()?
                Equals((Tuple<TFirst, TSecond>)obj):
                false;
        }

        /// <summary>
        /// Serves as a hash function for the underlying tuple type.
        /// </summary>
        /// <returns>Hash function result</returns>
        public override int GetHashCode()
        {
            return m_value.IsNull() ? 0 : m_value.GetHashCode();
        }

        /// <summary>
        /// Get string representation of the current tuple
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return m_value.IsNull() ? "" : m_value.ToString();
        }
    }
}
*/