using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.IServiceProvider class.
    /// </summary>
    [TypeExtender(typeof(IServiceProvider))]
    public static class IServiceProviderExtensions
    {
        /// <summary>
        /// Gets the service object of the specified type.
        /// </summary>
        /// <typeparam name="T">The type of service object to get.</typeparam>
        /// <param name="target"></param>
        /// <returns> A service object of type T.  -or- null if there is no service object
        ///     of type T.</returns>
        public static T GetService<T>(this IServiceProvider target)
        {
            return target.GetService(typeof(T)).SafeCast<T>();
        }
    }
}
