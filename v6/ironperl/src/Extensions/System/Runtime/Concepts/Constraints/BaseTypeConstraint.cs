using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Runtime.Concepts.Constraints
{
    using DebuggerDisplayAttribute = System.Diagnostics.DebuggerDisplayAttribute;

    /// <summary>
    /// Represents implementation of the constraint on the base type.
    /// </summary>
    /// <typeparam name="TBase">Base concept type.</typeparam>
    struct BaseTypeConstraint<TBase>: IBaseTypeConstraint
        where TBase : class
    {
        static BaseTypeConstraint()
        {
            if (typeof(TBase).IsInterface)
                throw new ArgumentException(Properties.Resources.String_ClassRequired, "TBase");
        }

        #region IDerivationConstraint Members

        /// <summary>
        /// Get metadata about base type of the contract.
        /// </summary>
        public Type InheritanceItem
        {
            get { return typeof(TBase); }
        }

        #endregion

        #region IEquatable<IConceptConstraint> Members

        bool IEquatable<IConceptConstraint>.Equals(IConceptConstraint other)
        {
            return Equals(other);
        }

        #endregion

        /// <summary>
        /// Determines whether the other object represents the same concept
        /// constraints as the current.
        /// </summary>
        /// <param name="obj">A System.Object, which represents concept constraint.</param>
        /// <returns>True, if the other object represents the same concept
        /// constraints as the current.</returns>
        public override bool Equals(object obj)
        {
            return Equals(obj as IBaseTypeConstraint);
        }

        /// <summary>
        /// Get System.Int32, which is uniequly identifies the current constraint.
        /// </summary>
        /// <returns>A System.Int32, which is uniequly identifies the current constraint.</returns>
        public override int GetHashCode()
        {
            return InheritanceItem.GetHashCode();
        }

        /// <summary>
        /// Determines whether the other object represents the same concept
        /// constraints as the current.
        /// </summary>
        /// <param name="other">A System.Object, which represents concept constraint.</param>
        /// <returns>True, if the other object represents the same concept
        /// constraints as the current.</returns>
        public bool Equals(IBaseTypeConstraint other)
        {
            return InheritanceItem.Equals(other.InheritanceItem);
        }
    }
}
