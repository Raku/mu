using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// Represents a null value for specified type
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [System.Runtime.CompilerServices.Singleton]
    public static class NullRef<T>
    {
        /// <summary>
        /// Represents a default value for specified type
        /// </summary>
        public static T Value
        {
            get { return default(T); }
        }
    }
}
