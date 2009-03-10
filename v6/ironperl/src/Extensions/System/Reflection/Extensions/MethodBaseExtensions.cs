using System.Extensions;
using System.Linq;

namespace System.Reflection.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Reflection.MethodBase class
    /// </summary>
    [TypeExtender(typeof(MethodBase))]
    public static class MethodBaseExtensions
    {
        public static T Invoke<T>(this MethodBase target, object owner, params object[] args)
        {
            ExceptionManager.CheckOnNull(target, "target");
            return (T)target.Invoke(owner, args);
        }

        public static Type[] GetParameterTypes(this MethodBase target)
        {
            var queryResult = from p in target.GetParameters()
                              select p.ParameterType;
            return queryResult.ToArray();
        }

        /// <summary>
        /// Get parameter information by it name
        /// </summary>
        /// <param name="target">Target method metadata</param>
        /// <param name="paramName">Parameter name</param>
        /// <returns>Parameter metadata</returns>
        public static ParameterInfo GetParameter(this MethodBase target, string paramName)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(paramName, "paramName");
            var queryResult = from p in target.GetParameters()
                              where p.Name == paramName
                              select p;
            return queryResult.FirstOrDefault();
        }
    }
}
