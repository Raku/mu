using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace System
{
    /// <summary>
    /// Marks the class as a extension provider for another class
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true, Inherited = false)]
    [ComVisible(true)]
    [Guid("B6C6D3F5-E96F-4e91-BD1B-663604A74D7D")]
    sealed class TypeExtenderAttribute: Attribute
    {
        private readonly Type m_targetClass;

        /// <summary>
        /// Create instance of the TypeExtenderAttribute attribute
        /// </summary>
        /// <param name="classToExtend">Metadata of the class to extend</param>
        public TypeExtenderAttribute(Type classToExtend)
        {
            m_targetClass = classToExtend;
        }

        /// <summary>
        /// Get metadata of the class, which is extended by SEL
        /// </summary>
        public Type ExtendedClass
        {
            get { return m_targetClass; }
        }
    }
}
