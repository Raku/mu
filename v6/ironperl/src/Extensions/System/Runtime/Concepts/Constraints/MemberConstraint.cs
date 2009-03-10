using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;
using System.Reflection.Extensions;

namespace System.Runtime.Concepts.Constraints
{
    using MemberInfo = System.Reflection.MemberInfo;

    /// <summary>
    /// Represents runtime description of the member.
    /// </summary>
    /// <typeparam name="TMember">Member type.</typeparam>
    abstract class MemberConstraint<TMember> : IMemberConstraint
        where TMember : MemberInfo
    {
        private readonly TMember m_memberInfo;

        /// <summary>
        /// Initialize MemberConstraint&lt;TMember&gt; class members.
        /// </summary>
        /// <param name="memberInfo">Member reference. Cannot be null.</param>
        /// <exception cref="System.ArgumentNullException">memberInfo is null.</exception>
        protected MemberConstraint(TMember memberInfo)
        {
            ExceptionManager.CheckOnNull(memberInfo, "memberInfo");
            m_memberInfo = memberInfo;
        }

        #region IMemberConstraint Members

        /// <summary>
        /// Get description of the member.
        /// </summary>
        public ConceptMemberAttribute Description
        {
            get 
            {
                var attr = MemberRef.GetCustomAttribute<ConceptMemberAttribute>();
                if (attr.IsNull())
                    attr = new ConceptMemberAttribute(MemberRef.Name) { IsPublic = true };
                if (attr.MemberName.IsNullOrEmpty())
                    attr = new ConceptMemberAttribute(MemberRef.Name) { IsPublic = attr.IsPublic };
                return attr;
            }
        }

        MemberInfo IMemberConstraint.MemberRef
        {
            get { return MemberRef; }
        }

        #endregion

        /// <summary>
        /// Get description of the member metadata.
        /// </summary>
        public TMember MemberRef
        {
            get { return m_memberInfo; }
        }

        #region IEquatable<IConceptConstraint> Members

        bool IEquatable<IConceptConstraint>.Equals(IConceptConstraint other)
        {
            return Equals(other);
        }

        #endregion

        public override bool Equals(object obj)
        {
            return base.Equals(obj);
        }

        /// <summary>
        /// Determines whether the other object represents the same concept
        /// constraints as the current.
        /// </summary>
        /// <param name="other">A System.Object, which represents concept constraint.</param>
        /// <returns>True, if the other object represents the same concept
        /// constraints as the current.</returns>
        public bool Equals(IMemberConstraint other)
        {
            if (other.IsNull()) return false;
            return false;
        }
    }
}
