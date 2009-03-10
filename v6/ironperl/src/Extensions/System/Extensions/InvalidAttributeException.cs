using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    /// <summary>
    /// Exception for case when specified metadata entity doesn't contain a specified attribute
    /// </summary>
    [Serializable]
    public sealed class InvalidAttributeException: Exception
    {
        private readonly Type m_expectedAttribute;

        public InvalidAttributeException(Type attributeType, Exception innerException)
            : base(String.Format(System.Properties.Resources.String_InvalidAttributeException, attributeType.Name), innerException)
        {
        }

        public InvalidAttributeException(Type attributeType)
            : this(attributeType, null)
        {
        }
    }
}
