using System.Extensions;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents extension methods for System.Collections.Processing.INavigationContext interface
    /// </summary>
    [TypeExtender(typeof(INavigationContext<>))]
    public static class INavigationContextExtensions
    {
        /// <summary>
        /// Increment navigator position
        /// </summary>
        /// <param name="navigator"></param>
        /// <returns></returns>
        public static object MoveNext<T>(this INavigationContext<T> navigator)
        {
            ExceptionManager.CheckOnNull(navigator, "navigator");
            if (navigator.Next.IsNull()) return null;
            return navigator.CurrentPosition = navigator.Next;
        }

        /// <summary>
        /// Decrement navigator position
        /// </summary>
        /// <param name="navigator"></param>
        /// <returns></returns>
        public static object MovePrev<T>(this INavigationContext<T> navigator)
        {
            ExceptionManager.CheckOnNull(navigator, "navigator");
            if (navigator.Previous.IsNull()) return null;
            return navigator.CurrentPosition = navigator.Previous;
        }
    }
}
