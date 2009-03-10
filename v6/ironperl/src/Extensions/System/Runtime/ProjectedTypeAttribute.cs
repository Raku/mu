using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime
{
    /// <summary>
    /// This API supports the SEL infrastructure 
    /// and is not intended to be used directly from your code.
    /// </summary>
    /// <remarks>Provides information about projection type. </remarks>
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false,
        Inherited = false)]
    public sealed class ProjectedTypeAttribute: Attribute
    {
        private readonly Type m_projectedType;
        private readonly Type m_targetType;

        public ProjectedTypeAttribute(Type projectedType, Type targetType)
        {
            m_projectedType = projectedType;
            m_targetType = targetType;
        }

        public Type ProjectedType
        {
            get { return m_projectedType; }
        }

        public Type TargetType
        {
            get { return m_targetType; }
        }
    }
}
