using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    /// <summary>
    /// Represents a name of the method, which overloads specified operator
    /// </summary>
    ///<remarks>This class is designed for System.OperatorKind class</remarks>
    [AttributeUsage(AttributeTargets.Field, AllowMultiple = false, Inherited=false)]
    sealed class OperatorMethodAttribute:Attribute
    {
        /// <summary>
        /// Represents an operator class
        /// </summary>
        public enum OperatorClass
        {
            Unary,
            Binary
        }

        private readonly string m_methodName;
        private readonly string m_operatorName;

        public OperatorMethodAttribute(string methodName, string operatorName)
        {
            m_methodName = methodName;
            m_operatorName = operatorName;
            TypeOfOperator = OperatorClass.Binary;
        }

        /// <summary>
        /// Get name of the method, which overloads a specified operator
        /// </summary>
        public string MethodName
        {
            get { return m_methodName; }
        }

        /// <summary>
        /// Get name of the operator
        /// </summary>
        public string OperatorName
        {
            get { return m_operatorName; }
        }

        public OperatorClass TypeOfOperator
        {
            get;
            set;
        }
    }
}
