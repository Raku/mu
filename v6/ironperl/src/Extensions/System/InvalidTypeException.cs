using System.Runtime.Serialization;

namespace System
{
    using ExceptionManager = global::System.Extensions.ExceptionManager;

    /// <summary>
    /// Exception for case when specified type is not valid
    /// </summary>
    [Serializable]
    public sealed class InvalidTypeException: Exception, ISerializable
    {
        private readonly Type m_expectedType;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private InvalidTypeException()
        {
        }

        public InvalidTypeException(Type expectedType, string message, Exception innerException)
            : base(CreateExceptionMessgae(expectedType, message), innerException)
        {
        }

        public InvalidTypeException(Type expectedType, string message)
            : base(CreateExceptionMessgae(expectedType, message))
        {
        }

        public InvalidTypeException(Type expectedType)
            : this(expectedType, null)
        {
            
        }

        private static string CreateExceptionMessgae(Type expectedType, string message)
        {
            ExceptionManager.CheckOnNull(expectedType, "expectedType");
            var result = String.Format(System.Properties.Resources.String_InvalidTypeException, expectedType.Name);
            if (RuntimeServices.IsNotNull(message))
                result = String.Format("{0}. {1}", message, result);
            return result;
        }
    }
}
