using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Reflection.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Reflection.ParameterInfo class.
    /// </summary>
    [TypeExtender(typeof(ParameterInfo))]
    public static class ParameterInfoExtensions
    {
        /// <summary>
        /// Get custom attributes for the specified parameter.
        /// </summary>
        /// <typeparam name="T">Required attribute type.</typeparam>
        /// <param name="param">Parameter metadata.</param>
        /// <returns>Array of attribute instances or empty array if attributes
        /// are not defined.</returns>
        public static T[] GetCustomAttributes<T>(this ParameterInfo param)
            where T : Attribute
        {
            var queryResult = from attr in Attribute.GetCustomAttributes(param, typeof(T))
                              select attr.SafeCast<T>();
            return queryResult.IsNotNull() ? queryResult.ToArray() : new T[0];
        }

        /// <summary>
        /// Determines whether any custom attributes are applied to a parameter.
        /// </summary>
        /// <typeparam name="T">The type, or a base type, of the custom attribute to search for.</typeparam>
        /// <param name="param">Target module.</param>
        /// <returns>true if a custom attribute of type T is applied to a parameter; otherwise, false.</returns>
        public static bool IsDefined<T>(ParameterInfo param)
            where T : Attribute
        {
            ExceptionManager.CheckOnNull(param, "param");
            //Performs a search
            return Attribute.IsDefined(param, typeof(T));
        }

        /// <summary>
        /// Get instance of the specified attribute.
        /// </summary>
        /// <typeparam name="T">Required attribute</typeparam>
        /// <param name="param">Target parameter.</param>
        /// <returns>Instance of the specified attribute</returns>
        public static T GetCustomAttribute<T>(this ParameterInfo param)
            where T : Attribute
        {
            return Attribute.GetCustomAttribute(param, typeof(T)).SafeCast<T>();
        }
    }
}
