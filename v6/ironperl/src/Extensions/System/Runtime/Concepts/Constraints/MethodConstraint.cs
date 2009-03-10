using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;

namespace System.Runtime.Concepts.Constraints
{
    /// <summary>
    /// Represents constraint, which describes the concept method.
    /// This class cannot be inherited.
    /// </summary>
    sealed class MethodConstraint : MemberConstraint<MethodInfo>
    {
        private const BindingFlags MethodFlags = BindingFlags.Instance |
            BindingFlags.Public |
            BindingFlags.NonPublic;

        /// <summary>
        /// Create constraint for the specified method.
        /// </summary>
        /// <param name="method">Target property metadata. Cannot be null.</param>
        public MethodConstraint(MethodInfo method)
            : base(method)
        {
        }

        /// <summary>
        /// Create constraint for the method with the specified name.
        /// </summary>
        /// <param name="value">Instance of the object, which contains the method.</param>
        /// <param name="methodName">Name of the method.</param>
        public MethodConstraint(object value, string methodName)
            : this(value.GetType().GetMethod(methodName, MethodFlags))
        {
        }
    }
}
