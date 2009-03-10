using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Concepts
{
    /// <summary>
    /// Represents description of the concept member.
    /// </summary>
    [Serializable]
    [AttributeUsage(AttributeTargets.Event | AttributeTargets.Method | AttributeTargets.Property, AllowMultiple = false, Inherited=true)]
    public sealed class ConceptMemberAttribute : Attribute
    {
        private readonly string m_memberName;

        /// <summary>
        /// Define a special name for the concept member.
        /// </summary>
        /// <param name="memberName">Name of the concept member.</param>
        /// <remarks>If memberName is null then Concept Loader extract
        /// name from member metadata.</remarks>
        public ConceptMemberAttribute(string memberName)
        {
            m_memberName = memberName;
            IsPublic = true;
        }

        /// <summary>
        /// Define a concept member.
        /// </summary>
        public ConceptMemberAttribute()
            : this(null)
        {
        }

        /// <summary>
        /// Get name of the concept member.
        /// </summary>
        public string MemberName
        {
            get { return m_memberName; }
        }

        /// <summary>
        /// Get or set value, which represents access level of the member.
        /// </summary>
        /// <remarks>If value is true then concept member requires a
        /// public equivalent in the object; otherwise, it expects
        /// member with any access level.</remarks>
        public bool IsPublic
        {
            get;
            set;
        }
    }
}
