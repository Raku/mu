using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Reflection.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Reflection.Module class.
    /// </summary>
    [TypeExtender(typeof(Module))]
    public static class ModuleExtensions
    {
        /// <summary>
        /// Get custom attributes for the specified module.
        /// </summary>
        /// <typeparam name="T">Required attribute type.</typeparam>
        /// <param name="module">Module metadata.</param>
        /// <returns>Array of attribute instances or empty array if attributes
        /// are not defined.</returns>
        public static T[] GetCustomAttributes<T>(this Module module)
            where T : Attribute
        {
            var queryResult = from attr in Attribute.GetCustomAttributes(module, typeof(T))
                              select attr.SafeCast<T>();
            return queryResult.IsNotNull() ? queryResult.ToArray() : new T[0];
        }

        /// <summary>
        /// Determines whether any custom attributes are applied to a module.
        /// </summary>
        /// <typeparam name="T">The type, or a base type, of the custom attribute to search for.</typeparam>
        /// <param name="target">Target module.</param>
        /// <returns>true if a custom attribute of type T is applied to module; otherwise, false.</returns>
        public static bool IsDefined<T>(this Module target)
            where T : Attribute
        {
            ExceptionManager.CheckOnNull(target, "target");
            //Performs a search
            return Attribute.IsDefined(target, typeof(T));
        }

        /// <summary>
        /// Get instance of the specified attribute
        /// </summary>
        /// <typeparam name="T">Required attribute</typeparam>
        /// <param name="target">Target module.</param>
        /// <returns>Instance of the specified attribute</returns>
        public static T GetCustomAttribute<T>(this Module target)
            where T : Attribute
        {
            return Attribute.GetCustomAttribute(target, typeof(T)).SafeCast<T>();
        }
    }
}
