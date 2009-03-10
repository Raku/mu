using System.Extensions;
using System.Linq;
using System.Runtime.Serialization;

namespace System.Reflection
{
    /// <summary>
    /// Represents a type information at runtime
    /// </summary>
    [Serializable]
    public struct RuntimeTypeInfo : ISerializable
    {
        private readonly RuntimeTypeHandle m_handle;
        private readonly FieldInfo[] m_fields;
        private readonly PropertyInfo[] m_properties;
        private readonly EventInfo[] m_events;
        private readonly MethodInfo[] m_methods;
        private readonly ConstructorInfo[] m_ctors;

        /// <summary>
        /// Deserialize the current instance.
        /// </summary>
        /// <param name="info">Stores all the data needed to serialize or deserialize an object.</param>
        /// <param name="context">Describes the source and destination of a given serialized stream, and provides
        ///     an additional caller-defined context.</param>
        private RuntimeTypeInfo(SerializationInfo info, StreamingContext context)
        {
            m_handle = (RuntimeTypeHandle)info.GetValue("m_handle", typeof(RuntimeTypeHandle));
            m_fields = info.GetValue("m_fields", typeof(FieldInfo[])) as FieldInfo[];
            m_properties = info.GetValue("m_properties", typeof(PropertyInfo[])) as PropertyInfo[];
            m_events = info.GetValue("m_events", typeof(EventInfo[])) as EventInfo[];
            m_methods = info.GetValue("m_methods", typeof(MethodInfo[])) as MethodInfo[];
            m_ctors = info.GetValue("m_ctors", typeof(ConstructorInfo[])) as ConstructorInfo[];
        }

        /// <summary>
        /// Get a metadata token of the type
        /// </summary>
        public RuntimeTypeHandle Handle
        {
            get { return m_handle; }
        }

        internal RuntimeTypeInfo(Type innerType)
        {
            m_handle = innerType.TypeHandle;
            m_fields = innerType.GetInstanceMembers<FieldInfo>(true).ToArray();
            m_properties = innerType.GetInstanceMembers<PropertyInfo>(false).ToArray();
            m_events = innerType.GetInstanceMembers<EventInfo>(false).ToArray();
            m_methods = innerType.GetInstanceMembers<MethodInfo>(true).ToArray();
            m_ctors = innerType.GetInstanceMembers<ConstructorInfo>(false).ToArray();   
        }

        /// <summary>
        /// Get all fields that are included into instance
        /// </summary>
        public FieldInfo[] Fields
        {
            get { return m_fields; }
        }

        /// <summary>
        /// Get all properties that are included into instance
        /// </summary>
        public PropertyInfo[] Properties
        {
            get { return m_properties; }
        }

        /// <summary>
        /// Get all events that are included into instance
        /// </summary>
        public EventInfo[] Events
        {
            get { return m_events; }
        }

        /// <summary>
        /// Get all methods that are included into instance
        /// </summary>
        public MethodInfo[] Methods
        {
            get { return m_methods; }
        }

        /// <summary>
        /// Get all constructors that are included into instance
        /// </summary>
        public ConstructorInfo[] Constructors
        {
            get { return m_ctors; }
        }

        #region ISerializable Members

        void ISerializable.GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("m_handle", m_handle, typeof(RuntimeTypeHandle));
            info.AddValue("m_fields", m_fields, typeof(FieldInfo[]));
            info.AddValue("m_properties", m_properties, typeof(PropertyInfo[]));
            info.AddValue("m_events", m_events, typeof(EventInfo[]));
            info.AddValue("m_methods", m_methods, typeof(MethodInfo[]));
            info.AddValue("m_ctors", m_ctors, typeof(ConstructorInfo[]));
        }

        #endregion
    }
}
