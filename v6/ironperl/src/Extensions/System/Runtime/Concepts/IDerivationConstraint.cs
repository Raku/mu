using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Concepts
{
    /// <summary>
    /// Represents constraint of the contract, which describes item
    /// in the inheritance list: interface implementation or base type.
    /// </summary>
    public interface IDerivationConstraint: IConceptConstraint
    {
        /// <summary>
        /// Get information about inheritance item.
        /// </summary>
        Type InheritanceItem { get; }
    }
}
