using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Concepts.Constraints
{
    /// <summary>
    /// Represents implementation of the constraint on the interface type.
    /// </summary>
    /// <typeparam name="TFace">Base concept type.</typeparam>
    struct InterfaceConstraint<TFace> : IInterfaceConstraint
        where TFace : class
    {
        static InterfaceConstraint()
        {
            if (!typeof(TFace).IsInterface)
                throw new ArgumentException(Properties.Resources.String_InterfaceRequired, "TFace");
        }

        #region IDerivationConstraint Members

        Type IDerivationConstraint.InheritanceItem
        {
            get { return typeof(TFace); }
        }

        #endregion

        #region IEquatable<IConceptConstraint> Members

        bool IEquatable<IConceptConstraint>.Equals(IConceptConstraint other)
        {
            throw new NotImplementedException();
        }

        #endregion
    }
}
