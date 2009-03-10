using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// Marks a class that it has a specified object factory
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Interface | AttributeTargets.Struct | AttributeTargets.Delegate, 
        AllowMultiple = false, 
        Inherited = false)]
    public sealed class ObjectProviderAttribute : Attribute
    {
        private readonly Type m_factory;

        /// <summary>
        /// Create instance of the ObjectFactoryAttribute and defines
        /// factory type
        /// </summary>
        /// <param name="factory"></param>
        public ObjectProviderAttribute(Type factory)
        {
            m_factory = factory;
        }

        /// <summary>
        /// Get object factory type
        /// </summary>
        public Type Factory
        {
            get { return m_factory; }
        }

        /// <summary>
        /// Create instance of the object factory
        /// </summary>
        /// <returns></returns>
        public IObjectFactory GetObjectFactory()
        {
            return Activator.CreateInstance(m_factory) as IObjectFactory;
        }
    }
}
