using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Collections.Extensions
{
    /// <summary>
    /// Represents extensions for the System.ICollection interface
    /// </summary>
    [TypeExtender(typeof(ICollection))]
    public static class ICollectionExtensions
    {
        /// <summary>
        /// Get value indicating that the current collection is empty
        /// </summary>
        /// <param name="self"></param>
        /// <returns>True, if collection is empty; otherwise false</returns>
        public static bool IsEmpty(this ICollection self)
        {
            return self.Count == 0;
        }
    }
}
