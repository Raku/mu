using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Concepts
{
    /// <summary>
    /// Represents functionality of the concept.
    /// </summary>
    /// <typeparam name="TBase">Type of the base class of the concept
    /// implementer.</typeparam>
    public interface IConcept<TBase>: IEquatable<IConcept<TBase>>
        where TBase : class
    {
        /// <summary>
        /// Unwrap instance of the concept implementer from
        /// the concept instance.
        /// </summary>
        TBase BaseObject { get; }

        /// <summary>
        /// Get runtime information about constraints in the current
        /// concept.
        /// </summary>
        ConceptConstraintCollection Constraints { get; }
        
        /// <summary>
        /// Converts the current concept into the object of the specified
        /// type.
        /// </summary>
        /// <typeparam name="T">Target type.</typeparam>
        /// <returns>Instance of the specified type.</returns>
        /// <remarks>T represents one of the implemented interface
        /// or concept base type.</remarks>
        T QueryObject<T>()
            where T : class;
    }
}
