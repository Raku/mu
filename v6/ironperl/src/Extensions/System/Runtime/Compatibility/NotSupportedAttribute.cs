using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Compatibility
{
    /// <summary>
    /// Marks method or constructor that is not supported and doesn't
    /// provider an implementation. If method or constructor (which is marked
    /// by this attribute) is invoked then it should throws the 
    /// System.NotSupportedException exception.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Constructor)]
    [Serializable]
    public sealed class NotSupportedAttribute : Attribute
    {
        private readonly string m_reason;

        /// <summary>
        /// Create instance of the System.NotSupportedAttribute attribute.
        /// </summary>
        /// <param name="reason">Human-readable string, which explains
        /// unsupported state of the method or constructor.</param>
        public NotSupportedAttribute(string reason)
        {
            m_reason = reason;
        }

        /// <summary>
        /// Create instance of the System.NotSupportedAttribute attribute
        /// with the empty explanation.
        /// </summary>
        public NotSupportedAttribute()
            : this(String.Empty)
        {
        }

        /// <summary>
        /// Get value, which explains unsupported state of the method.
        /// </summary>
        public string Reason
        {
            get { return m_reason; }
        }
    }
}
