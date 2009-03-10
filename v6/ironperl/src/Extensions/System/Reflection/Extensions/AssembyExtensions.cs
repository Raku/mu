using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Reflection.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Reflection.Assembly class.
    /// </summary>
    [TypeExtender(typeof(Assembly))]
    public static class AssembyExtensions
    {
        /// <summary>
        /// Get custom attributes for the specified assembly.
        /// </summary>
        /// <typeparam name="T">Required attribute type.</typeparam>
        /// <param name="asm">Assembly metadata.</param>
        /// <returns>Array of attribute instances or empty array if attributes
        /// are not defined.</returns>
        public static T[] GetCustomAttributes<T>(this Assembly asm)
            where T : Attribute
        {
            var queryResult = from attr in Attribute.GetCustomAttributes(asm, typeof(T))
                              select attr.SafeCast<T>();
            return queryResult.IsNotNull() ? queryResult.ToArray() : new T[0];
        }

        /// <summary>
        /// Determines whether any custom attributes are applied to an assembly.
        /// </summary>
        /// <typeparam name="T">The type, or a base type, of the custom attribute to search for.</typeparam>
        /// <param name="asm">Target assembly.</param>
        /// <returns>true if a custom attribute of type T is applied to assembly; otherwise, false.</returns>
        public static bool IsDefined<T>(this Assembly asm)
            where T : Attribute
        {
            ExceptionManager.CheckOnNull(asm, "asm");
            //Performs a search
            return Attribute.IsDefined(asm, typeof(T));
        }

        /// <summary>
        /// Get instance of the specified attribute
        /// </summary>
        /// <typeparam name="T">Required attribute</typeparam>
        /// <param name="asm">Target assembly.</param>
        /// <returns>Instance of the specified attribute</returns>
        public static T GetCustomAttribute<T>(this Assembly asm)
            where T : Attribute
        {
            return Attribute.GetCustomAttribute(asm, typeof(T)).SafeCast<T>();
        }
    }
}
