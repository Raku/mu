using System.Collections.Generic;
using System.Extensions;

namespace System.Collections.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Collections.IEnumerable interface
    /// </summary>
    [TypeExtender(typeof(IEnumerable))]
    public static class IEnumerableExtensions
    {
        /// <summary>
        /// Change type of the sequence elements
        /// </summary>
        /// <typeparam name="TResult">A new sequence element type</typeparam>
        /// <param name="target">Sequence to convert</param>
        /// <returns>Converted sequence</returns>
        public static IEnumerable<TResult> Convert<TResult>(this IEnumerable target)
        {
            if (target.IsNotNull())
                foreach (var item in target)
                    yield return item.UnsafeCast<TResult>();
        }
    }
}
