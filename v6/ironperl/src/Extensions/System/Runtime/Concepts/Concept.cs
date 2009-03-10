using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;
using System.Reflection;

namespace System.Runtime.Concepts
{
    using Constraints;
    using BindingFlags = System.Reflection.BindingFlags;

    /// <summary>
    /// Provides methods, which are support the Coneptual Programming Model.
    /// </summary>
    public static class Concept
    {
        /// <summary>
        /// Compare two concepts. If they are represents the same costraint set
        /// then returns true.
        /// </summary>
        /// <typeparam name="TBase">Base type for the concept.</typeparam>
        /// <typeparam name="TConcept1">Type of the first concept.</typeparam>
        /// <typeparam name="TConcept2">Type of the second concept.</typeparam>
        /// <returns>True, if both concept types are represent the same
        /// constraint set; otherwise, false.</returns>
        public static bool Equal<TBase, TConcept1, TConcept2>()
            where TBase : class
            where TConcept1 : IConcept<TBase>
            where TConcept2 : IConcept<TBase>
        {
            var constraints1 = CreateConstraints<TBase, TConcept1>();
            var constraints2 = CreateConstraints<TBase, TConcept2>();
            return constraints1.Equals(constraints2);
        }

        /// <summary>
        /// Create description of the concept constraints.
        /// </summary>
        /// <typeparam name="TBase">Base type of the concept.</typeparam>
        /// <typeparam name="TConcept">Type of the contract.</typeparam>
        /// <returns>Collection of the concept constraints.</returns>
        public static ConceptConstraintCollection CreateConstraints<TBase, TConcept>()
            where TBase : class
            where TConcept : IConcept<TBase>
        {
            return CreateConstraints<TBase>(typeof(TConcept));
        }

        internal static ConceptConstraintCollection CreateConstraints<TBase>(Type conceptType)
            where TBase : class
        {
            const BindingFlags memberFlags = BindingFlags.Public | BindingFlags.Instance;
            var result = new List<IConceptConstraint>();
            //Add base type definition:
            result.Add(new BaseTypeConstraint<TBase>());
            //Iterate through all members and create constraint for it.
            foreach (var member in conceptType.GetMembers(memberFlags))
                switch (member.MemberType)
                {
                    case MemberTypes.Property:
                        result.Add(new PropertyConstraint((PropertyInfo)member)); break;
                    case MemberTypes.Method:
                        result.Add(new MethodConstraint((MethodInfo)member)); break;
                    case MemberTypes.Event:
                        result.Add(new EventConstraint((EventInfo)member)); break;
                }
            //Iterate through all interfaces and constraint for it.
            foreach (var iface in conceptType.GetInterfaces())
            {
                if (IsConceptInterface<TBase>(iface)) continue;
                var constrType = typeof(InterfaceConstraint<>);
                constrType = constrType.MakeGenericType(iface);
                result.Add((IConceptConstraint)Activator.CreateInstance(constrType));
            }
            return new ConceptConstraintCollection(result);
        }

        private static bool IsConceptInterface<TBase>(Type iface)
            where TBase : class
        {
            var conceptBase = typeof(IConcept<TBase>);
            if (conceptBase.Equals(iface)) return true;
            foreach (var conceptItem in conceptBase.GetInterfaces())
                if (iface.Equals(conceptItem)) return true;
            return false;
        }
    }
}
