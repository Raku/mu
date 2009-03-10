using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Collections.Generic.Extensions
{
    /// <summary>
    /// Represents extension methods for System.Collections.Generic.IList interface
    /// </summary>
    [TypeExtender(typeof(IList<>))]
    public static class IListExtensions
    {
        public static void AddItems<T>(this IList<T> target, params T[] items)
        {
            ExceptionManager.CheckOnNull(target, "target");
            if (items.IsNull()) return;
            foreach (var item in items)
                target.Add(item);
        }
    }
}
