using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.ConstrainedExecution;
using System.Reflection;
using System.Reflection.Emit;
using System.Extensions;

namespace System.Runtime
{
    /// <summary>
    /// Represents Instance Projection provider
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public sealed class Projection<T, G> : CriticalFinalizerObject, IConvertible<T>
        where T : class, IProjection<G>
        where G : class
    {
        private readonly object m_value;

        /// <summary>
        /// Create Instance Projection for the specified object
        /// </summary>
        /// <param name="value"></param>
        public Projection(object value)
        {
            
            ExceptionManager.CheckOnNull(value, "value");
            m_value = value;
            //current value must be compatible with declared contract
            foreach (var iface in typeof(T).GetInterfaces())
            {
                if (iface.Equals(typeof(IProjection<G>))) continue;
                if (!value.GetType().CanCastTo(iface))
                    ExceptionManager.Throw<InvalidOperationException>(Properties.Resources.String_InvalidContract);
            }
        }

        #region IConvertible<T> Members

        /// <summary>
        /// Get projection that is represented by the current instance
        /// </summary>
        /// <returns>Instance Projection</returns>
        [ReliabilityContract(Consistency.MayCorruptAppDomain, Cer.MayFail)]
        public T Convert()
        {
            var stubType = ProjectionHost.Instance.CreateTypeStub(typeof(T), m_value.GetType());
            return Activator.CreateInstance(stubType, m_value) as T;
        }

        #endregion
    }
}
