using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Concepts
{
    using MemberInfo = System.Reflection.MemberInfo;

    /// <summary>
    /// Represents description of the concept member.
    /// </summary>
    public interface IMemberConstraint: IConceptConstraint
    {
        /// <summary>
        /// Get runtime description of the member.
        /// </summary>
        ConceptMemberAttribute Description { get; }

        /// <summary>
        /// Get member reference.
        /// </summary>
        MemberInfo MemberRef { get; }
    }
}
