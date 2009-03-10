using System.Linq;
using System.Extensions;

namespace System.Reflection.Extensions
{
    /// <summary>
    /// Represents an extensions for the System.Reflection.MemberInfo class
    /// </summary>
    [TypeExtender(typeof(MemberInfo))]
    public static class MemberInfoExtensions
    {
        /// <summary>
        /// Get custom attributes for the specified member metadata
        /// </summary>
        /// <typeparam name="T">Required attribute type</typeparam>
        /// <param name="member">Member metadata</param>
        /// <returns>Array of attribute instances or empty array if attributes
        /// are not defined.</returns>
        public static T[] GetCustomAttributes<T>(this MemberInfo member)
            where T : Attribute
        {
            var queryResult = from attr in Attribute.GetCustomAttributes(member, typeof(T))
                              select attr.SafeCast<T>();
            return queryResult.IsNotNull() ? queryResult.ToArray() : new T[0];
        }

        /// <summary>
        /// Determines whether any custom attributes are applied to a member.
        /// </summary>
        /// <typeparam name="T">The type, or a base type, of the custom attribute to search for.</typeparam>
        /// <param name="member">Target member</param>
        /// <returns>true if a custom attribute of type T is applied to assembly; otherwise, false.</returns>
        public static bool IsDefined<T>(this MemberInfo member)
            where T : Attribute
        {
            ExceptionManager.CheckOnNull(member, "member");
            //Performs a search
            return Attribute.IsDefined(member, typeof(T));
        }

        /// <summary>
        /// Get instance of the specified attribute
        /// </summary>
        /// <typeparam name="T">Required attribute</typeparam>
        /// <param name="target">Target member</param>
        /// <returns>Instance of the specified attribute</returns>
        public static T GetCustomAttribute<T>(this MemberInfo target)
            where T : Attribute
        {
            return Attribute.GetCustomAttribute(target, typeof(T)).SafeCast<T>();
        }
    }
}
