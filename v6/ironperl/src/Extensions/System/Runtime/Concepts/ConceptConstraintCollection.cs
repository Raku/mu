using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections.ObjectModel;
using System.Extensions;

namespace System.Runtime.Concepts
{
    /// <summary>
    /// Represents read-only collection of the concept constraints.
    /// </summary>
    public sealed class ConceptConstraintCollection: ReadOnlyCollection<IConceptConstraint>, IEquatable<ConceptConstraintCollection>
    {
        private struct ConstraintComparer : IEqualityComparer<IConceptConstraint>
        {
            #region IEqualityComparer<IConceptConstraint> Members

            public bool Equals(IConceptConstraint x, IConceptConstraint y)
            {
                return x.IsNotNull() ? x.Equals(y) : y.IsNotNull() ? y.Equals(x) : false;
            }

            public int GetHashCode(IConceptConstraint obj)
            {
                return obj.GetHashCode();
            }

            #endregion
        }

        /// <summary>
        /// Create collection of the concept constraints.
        /// </summary>
        /// <param name="constraints"></param>
        public ConceptConstraintCollection(IEnumerable<IConceptConstraint> constraints)
            : base(new List<IConceptConstraint>(constraints))
        {
        }

        /// <summary>
        /// Get sequence of the strongly typed constraints.
        /// </summary>
        /// <typeparam name="T">Type of the constraint.</typeparam>
        /// <returns>Constraints of the requested type.</returns>
        public IEnumerable<T> GetConstraints<T>()
            where T : IConceptConstraint
        {
            foreach (var constraint in this)
                if (constraint.GetType().IsAssignableFrom(typeof(T)))
                    yield return constraint.UnsafeCast<T>();
        }

        #region IEquatable<ConceptConstraintCollection> Members

        bool IEquatable<ConceptConstraintCollection>.Equals(ConceptConstraintCollection other)
        {
            return Equals(other);
        }

        #endregion

        /// <summary>
        /// Determines whether two constraint sets are equal.
        /// </summary>
        /// <param name="other">Constraint set to be compared.</param>
        /// <returns>True, if bot constraint sets are equal; otherwise, false.</returns>
        public bool Equals(IEnumerable<IConceptConstraint> other)
        {
            if (other.IsNull()) return false;
            foreach (var constraint1 in this)
            {
                var subResult = false;
                foreach (var constraint2 in other)
                    subResult |= constraint1.Equals(constraint2);
                if (!subResult) return false;
            }
            return true;
        }

        /// <summary>
        /// Determines whether two constraint sets are equal.
        /// </summary>
        /// <param name="other">Constraint set to be compared.</param>
        /// <returns>True, if bot constraint sets are equal; otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            return Equals(obj as IEnumerable<IConceptConstraint>);
        }

        /// <summary>
        /// Returns System.Int32 value, which uniquely identifies the current instance.
        /// </summary>
        /// <returns>A System.Int32 value, which uniquely identifies the current instance.</returns>
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        /// <summary>
        /// Provides intersection of the two constraint sets.
        /// </summary>
        /// <param name="other">Second constraint set.</param>
        /// <returns>Intersection between two constraint sets.</returns>
        /// <exception cref="System.ArgumentNullException">other is null.</exception>
        public ConceptConstraintCollection Intersect(IEnumerable<IConceptConstraint> other)
        {
            ExceptionManager.CheckOnNull(other, "other");
            var result = this.Intersect(other, new ConstraintComparer());
            return new ConceptConstraintCollection(result);
        }

        /// <summary>
        /// Provides union of the two constraint sets.
        /// </summary>
        /// <param name="other">Second constraint set.</param>
        /// <returns>Union between two constraint sets.</returns>
        /// <exception cref="System.ArgumentNullException">other is null.</exception>
        public ConceptConstraintCollection Union(IEnumerable<IConceptConstraint> other)
        {
            ExceptionManager.CheckOnNull(other, "other");
            var result = this.Union(other, new ConstraintComparer());
            return new ConceptConstraintCollection(result);
        }

        /// <summary>
        /// Produces the constraint set difference.
        /// </summary>
        /// <param name="other">Second constraint set.</param>
        /// <returns>constraint set difference.</returns>
        /// <exception cref="System.ArgumentNullException">other is null.</exception>
        public ConceptConstraintCollection Except(IEnumerable<IConceptConstraint> other)
        {
            ExceptionManager.CheckOnNull(other, "other");
            var result = this.Except(other, new ConstraintComparer());
            return new ConceptConstraintCollection(result);
        }

        /// <summary>
        /// Provides intersection of the two constraint sets.
        /// </summary>
        /// <param name="set1">The first constraint set.</param>
        /// <param name="set2">The second constraint set.</param>
        /// <returns>Intersection between two constraint sets.</returns>
        public static ConceptConstraintCollection operator &(ConceptConstraintCollection set1, IEnumerable<IConceptConstraint> set2)
        {
            if (set1.IsNull())
                return new ConceptConstraintCollection(new IConceptConstraint[0]);
            if (set2.IsNull())
                return new ConceptConstraintCollection(new IConceptConstraint[0]);
            return set1.Intersect(set2);
        }

        /// <summary>
        /// Provides union of the two constraint sets.
        /// </summary>
        /// <param name="set1">The first constraint set.</param>
        /// <param name="set2">The second constraint set.</param>
        /// <returns>Union of the two constraint sets.</returns>
        public static ConceptConstraintCollection operator |(ConceptConstraintCollection set1, IEnumerable<IConceptConstraint> set2)
        {
            if (set1.IsNull())
                return new ConceptConstraintCollection(new IConceptConstraint[0]);
            if (set2.IsNull())
                return new ConceptConstraintCollection(new IConceptConstraint[0]);
            return set1.Union(set2);
        }

        /// <summary>
        /// Produces the constraint set difference.
        /// </summary>
        /// <param name="set1">The first constraint set.</param>
        /// <param name="set2">The second constraint set.</param>
        /// <returns>Constraint set difference.</returns>
        public static ConceptConstraintCollection operator ^(ConceptConstraintCollection set1, IEnumerable<IConceptConstraint> set2)
        {
            if (set1.IsNull())
                return new ConceptConstraintCollection(new IConceptConstraint[0]);
            if (set2.IsNull())
                return new ConceptConstraintCollection(new IConceptConstraint[0]);
            return set1.Except(set2);
        }
    }
}
