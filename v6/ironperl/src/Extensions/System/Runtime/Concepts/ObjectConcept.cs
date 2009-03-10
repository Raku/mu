using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Runtime.Concepts
{
    using BindingFlags = System.Reflection.BindingFlags;

    /// <summary>
    /// Represents runtime representation of the concept.
    /// This class is used by the Runtime Concept Loader and
    /// is not allowed for direct inheritance.
    /// </summary>
    /// <typeparam name="TBase">Type of the concept inheritor.</typeparam>
    public abstract class ObjectConcept<TBase> : IConcept<TBase>
        where TBase : class
    {
        private readonly TBase m_baseValue;
        private ConceptConstraintCollection m_constraints;

        /// <summary>
        /// Initialize runtime concept and define object value.
        /// </summary>
        /// <param name="baseValue">Object value. Cannot be null.</param>
        /// <exception cref="System.ArgumentNullException">baseValue is null.</exception>
        protected ObjectConcept(TBase baseValue)
        {
            ExceptionManager.CheckOnNull(baseValue, "baseValue");
            m_baseValue = baseValue;
        }

        #region IConcept<TBase> Members

        TBase IConcept<TBase>.BaseObject
        {
            get { return BaseObject; }
        }

        ConceptConstraintCollection IConcept<TBase>.Constraints
        {
            get 
            {
                return Constraints;
            }
        }

        T IConcept<TBase>.QueryObject<T>()
        {
            return QueryObject<T>();
        }

        #endregion

        /// <summary>
        /// Get instance of the wrapped object.
        /// </summary>
        internal protected TBase BaseObject
        {
            get { return m_baseValue; }
        }

        /// <summary>
        /// Get constraints of the current collection.
        /// </summary>
        internal protected ConceptConstraintCollection Constraints
        {
            get 
            {
                if (m_constraints.IsNull())
                    m_constraints = Concept.CreateConstraints<TBase>(GetType());
                return m_constraints;
            }
        }

        /// <summary>
        /// Get string representation of the wrapped value.
        /// </summary>
        /// <returns>A System.String, which represents the wrapped value.</returns>
        public sealed override string ToString()
        {
            return BaseObject.ToString();
        }

        /// <summary>
        /// Determines whether two wrapped objects are equal.
        /// </summary>
        /// <param name="obj">A System.Object to compare;</param>
        /// <returns>True, if wrapped objects are equal; otherwise, false.</returns>
        public sealed override bool Equals(object obj)
        {
            if (obj is IConcept<TBase>)
                obj = obj.UnsafeCast<IConcept<TBase>>().BaseObject;
            return BaseObject.Equals(obj);
        }

        /// <summary>
        /// Serves as a hash function for a particular TBase type. 
        /// </summary>
        /// <returns>A hash code for the current wrapped object.</returns>
        public sealed override int GetHashCode()
        {
            return BaseObject.GetHashCode();
        }

        public static implicit operator TBase(ObjectConcept<TBase> concept)
        {
            return concept.BaseObject;
        }

        #region IEquatable<IConcept<TBase>> Members

        bool IEquatable<IConcept<TBase>>.Equals(IConcept<TBase> other)
        {
            throw new NotImplementedException();
        }

        #endregion

        /// <summary>
        /// Converts the underlying concept into specified type.
        /// </summary>
        /// <typeparam name="T">Type of result.</typeparam>
        /// <returns></returns>
        internal protected T QueryObject<T>()
            where T : class
        {
            if (BaseObject is T) return BaseObject.UnsafeCast<T>();
            return this is T ? this.UnsafeCast<T>() : default(T);
        }
    }
}
