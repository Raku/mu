using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System
{
    /// <summary>
    /// Represents boxed version of the target valuetype.
    /// </summary>
    /// <typeparam name="T">Target valuetype.</typeparam>
    /// <remarks>
    /// This class can be used for allocating valuetypes on the heap.
    /// </remarks>
    [Serializable]
    public sealed class Box<T>
        where T : struct
    {
        /// <summary>
        /// Create boxed version of the specified valuetype.
        /// </summary>
        /// <param name="value">Structure.</param>
        public Box(T value)
        {
            Value = value;
        }

        /// <summary>
        /// Create boxed version of the T type default value.
        /// </summary>
        public Box()
            : this(default(T))
        {
        }

        /// <summary>
        /// Get or set structure.
        /// </summary>
        public T Value
        {
            get;
            set;
        }

        /// <summary>
        /// Get string representation of the boxed value.
        /// </summary>
        /// <returns>String representation of the boxed value.</returns>
        public override string ToString()
        {
            return Value.ToString();
        }

        /// <summary>
        /// Determines whether the current boxed value represents the same structure
        /// as the other value.
        /// </summary>
        /// <param name="obj">Object to be compared.</param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            return Value.Equals(obj);
        }

        /// <summary>
        /// Returns a hash value of the structure, which is stored in the current
        /// instance.
        /// </summary>
        /// <returns>A hash value of the boxed valuetype.</returns>
        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public static implicit operator T(Box<T> boxedValue)
        {
            return boxedValue.IsNotNull() ? boxedValue.Value : default(T);
        }

        public static implicit operator Box<T>(T structure)
        {
            return new Box<T>(structure);
        }
    }
}
