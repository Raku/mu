using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;

namespace System.Runtime.Concepts.Constraints
{
    /// <summary>
    /// Represents constraint, which describes the concept property.
    /// This class cannot be inherited.
    /// </summary>
    sealed class PropertyConstraint : MemberConstraint<PropertyInfo>
    {
        private const BindingFlags PropertyFlags = BindingFlags.Instance | 
            BindingFlags.Public | 
            BindingFlags.NonPublic;

        /// <summary>
        /// Create constraint for the specified property.
        /// </summary>
        /// <param name="property">Target property metadata. Cannot be null.</param>
        public PropertyConstraint(PropertyInfo property)
            : base(property)
        {
        }

        /// <summary>
        /// Create constraint for the property with the specified name.
        /// </summary>
        /// <param name="value">Instance of the object, which contains property.</param>
        /// <param name="propertyName">Name of the property.</param>
        public PropertyConstraint(object value, string propertyName)
            : this(value.GetType().GetProperty(propertyName, PropertyFlags))
        {
        }
    }
}
