using System.Extensions;

namespace System
{
    /// <summary>
    /// Represents the object reference, which cannot be a null reference
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public struct NotNull<T>
        where T : class
    {
        private T m_value;

        /// <summary>
        /// Create instance of the safe reference
        /// </summary>
        /// <param name="value"></param>
        public NotNull(T value)
        {
            ExceptionManager.CheckOnNull(m_value = value, "value");
        }

        /// <summary>
        /// Get or set value of the safe reference
        /// </summary>
        /// <exception cref="System.ArgumentNullException">Specified reference is null</exception>
        public T Value
        {
            get { return m_value; }
            set
            {
                ExceptionManager.CheckOnNull(value, "value");
                m_value = value;
            }
        }

        public static implicit operator NotNull<T>(T value)
        {
            return new NotNull<T>(value);
        }

        public static implicit operator T(NotNull<T> notNull)
        {
            return notNull.Value;
        }

        /// <summary>
        ///  Returns a System.String that represents the T object.
        /// </summary>
        /// <returns>A System.String that represents the T object.</returns>
        public override string ToString()
        {
            return m_value.ToString();
        }

        /// <summary>
        ///  Determines whether the specified System.Object is equal to the current value.
        /// </summary>
        /// <param name="obj">The System.Object to compare with the current value.</param>
        /// <returns>true if the specified System.Object is equal to the current value;
        /// otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            return m_value.Equals(obj);
        }

        /// <summary>
        /// Serves as a hash function for a T type.
        /// </summary>
        /// <returns>A hash code for the instance of a T type.</returns>
        public override int GetHashCode()
        {
            return m_value.GetHashCode();
        }
    }
}


