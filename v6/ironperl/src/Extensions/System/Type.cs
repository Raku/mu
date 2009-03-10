using System.Collections.Generic;
using System.Extensions;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using CultureInfo = System.Globalization.CultureInfo;
using IType = System.Runtime.InteropServices._Type;

namespace System
{
    using FormatterServices = System.Runtime.Serialization.FormatterServices;

    /// <summary>
    /// Represents a metadata about concrete type
    /// </summary>
    /// <typeparam name="T">Target type</typeparam>
    [Serializable]
    public sealed class Type<T>: Type, IEnumerable<FieldInfo>, IEnumerable<PropertyInfo>, IEnumerable<EventInfo>, 
        IEnumerable<MethodInfo>,
        IEnumerable<Type>,
        IEnumerable<ConstructorInfo>,
        IEquatable<Type>
    {
        private const BindingFlags AnyMember = BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static;
        
        [NonSerialized]
        private readonly IType m_wrapper;
        private static readonly RuntimeTypeInfo m_runtimeInfo;

        static Type()
        {
            m_runtimeInfo = new RuntimeTypeInfo(typeof(T));
        }

        /// <summary>
        /// Create a type information
        /// </summary>
        private Type()
        {
            m_wrapper = typeof(T);
        }

        public override global::System.Reflection.Assembly Assembly
        {
            get { return m_wrapper.Assembly; }
        }

        public override string AssemblyQualifiedName
        {
            get { return m_wrapper.AssemblyQualifiedName; }
        }

        public override Type BaseType
        {
            get 
            {
                var baseType = m_wrapper.BaseType;
                if (RuntimeServices.IsNull(baseType)) return baseType;
                var result = typeof(Type<>).MakeGenericType(baseType);
                return Activator.CreateInstance(result) as Type;
            }
        }

        public override string FullName
        {
            get { return m_wrapper.FullName; }
        }

        public override Guid GUID
        {
            get { return m_wrapper.GUID; }
        }

        protected override TypeAttributes GetAttributeFlagsImpl()
        {
            return m_wrapper.Attributes;
        }

        protected override ConstructorInfo GetConstructorImpl(BindingFlags bindingAttr, Binder binder, CallingConventions callConvention, Type[] types, ParameterModifier[] modifiers)
        {
            var ctor = m_wrapper.GetConstructor(bindingAttr, binder, callConvention, types, modifiers);
            return OnResolveConstructor(ctor);
        }

        public override ConstructorInfo[] GetConstructors(BindingFlags bindingAttr)
        {
            return InternalGetConstructors(bindingAttr).ToArray();
        }

        private IEnumerable<ConstructorInfo> InternalGetConstructors(BindingFlags bindingAttr)
        {
            foreach (var ctor in m_wrapper.GetConstructors(bindingAttr))
            {
                var result = OnResolveConstructor(ctor);
                if (result.IsNotNull()) yield return result;
            }
        }

        public event MetadataParser<ConstructorInfo> ResolveConstructor;
        public event MetadataParser<EventInfo> ResolveEvent;
        public event MetadataParser<PropertyInfo> ResolveProperty;
        public event MetadataParser<FieldInfo> ResolveField;
        public event MetadataParser<Type> ResolveNestedType;
        public event MetadataParser<Type> ResolveInterface;
        public event MetadataParser<MethodInfo> ResolveMethod;

        private ConstructorInfo OnResolveConstructor(ConstructorInfo ctor)
        {
            if (ResolveConstructor != null)
                return ResolveConstructor(ctor);
            return ctor;
        }

        private EventInfo OnResolveEvent(EventInfo info)
        {
            if (ResolveEvent != null)
                return ResolveEvent(info);
            return info;
        }

        private PropertyInfo OnResolveProperty(PropertyInfo info)
        {
            if (ResolveProperty != null)
                return ResolveProperty(info);
            return info;
        }

        private FieldInfo OnResolveField(FieldInfo info)
        {
            if (ResolveField != null)
                return ResolveField(info);
            return info;
        }

        private Type OnResolveNestedType(Type info)
        {
            if (ResolveNestedType != null)
                return ResolveNestedType(info);
            return info;
        }

        private Type OnResolveInterface(Type iface)
        {
            if (ResolveInterface != null)
                return ResolveInterface(iface);
            return iface;
        }

        private MethodInfo OnResolveMethod(MethodInfo method)
        {
            if (ResolveMethod != null)
                return ResolveMethod(method);
            return method;
        }

        public override Type GetElementType()
        {
            return m_wrapper.GetElementType();
        }

        public override EventInfo GetEvent(string name, BindingFlags bindingAttr)
        {
            var eventInfo = m_wrapper.GetEvent(name, bindingAttr);
            return OnResolveEvent(eventInfo);
        }

        public override EventInfo[] GetEvents(BindingFlags bindingAttr)
        {
            return InternalGetEvents(bindingAttr).ToArray();
        }

        private IEnumerable<EventInfo> InternalGetEvents(BindingFlags bindingAttr)
        {
            foreach (var e in m_wrapper.GetEvents(bindingAttr))
            {
                var result = OnResolveEvent(e);
                if (e.IsNotNull()) yield return result;
            }
        }

        public override FieldInfo GetField(string name, BindingFlags bindingAttr)
        {
            var fieldInfo = m_wrapper.GetField(name, bindingAttr);
            return OnResolveField(fieldInfo);
        }

        public override FieldInfo[] GetFields(BindingFlags bindingAttr)
        {
            return InternalGetFields(bindingAttr).ToArray();
        }

        private IEnumerable<FieldInfo> InternalGetFields(BindingFlags bindingAttr)
        {
            foreach (var fld in m_wrapper.GetFields(bindingAttr))
            {
                var result = OnResolveField(fld);
                if (result.IsNotNull()) yield return result;
            }
        }

        public override Type GetInterface(string name, bool ignoreCase)
        {
            var result = m_wrapper.GetInterface(name, ignoreCase);
            return OnResolveInterface(result);
        }

        private IEnumerable<Type> InternalGetInterfaces()
        {
            foreach (var iface in m_wrapper.GetInterfaces())
            {
                var result = OnResolveInterface(iface);
                if (result.IsNotNull()) yield return result;
            }
        }

        public override Type[] GetInterfaces()
        {
            return InternalGetInterfaces().ToArray();
        }

        public override MemberInfo[] GetMembers(BindingFlags bindingAttr)
        {
            return InternalGetMembers(bindingAttr).ToArray();
        }

        private IEnumerable<MemberInfo> InternalGetMembers(BindingFlags bindingAttr)
        {
            foreach(var member in m_wrapper.GetMembers(bindingAttr))
                switch (member.MemberType)
                {
                    case MemberTypes.Constructor:
                        yield return OnResolveConstructor(member as ConstructorInfo); break;
                    case MemberTypes.Event:
                        yield return OnResolveEvent(member as EventInfo); break;
                    case MemberTypes.Field:
                        yield return OnResolveField(member as FieldInfo); break;
                    case MemberTypes.Method:
                        yield return OnResolveMethod(member as MethodInfo); break;
                    case MemberTypes.NestedType:
                        yield return OnResolveNestedType(member as Type); break;
                    case MemberTypes.Property:
                        yield return OnResolveProperty(member as PropertyInfo); break;
                }
        }

        protected override MethodInfo GetMethodImpl(string name, BindingFlags bindingAttr, Binder binder, CallingConventions callConvention, Type[] types, ParameterModifier[] modifiers)
        {
            var result = m_wrapper.GetMethod(name, bindingAttr, binder, callConvention, types, modifiers);
            return OnResolveMethod(result);
        }

        public override MethodInfo[] GetMethods(BindingFlags bindingAttr)
        {
            return InternalGetMethods(bindingAttr).ToArray();
        }

        private IEnumerable<MethodInfo> InternalGetMethods(BindingFlags bindingAttr)
        {
            foreach (var method in m_wrapper.GetMethods(bindingAttr))
            {
                var result = OnResolveMethod(method);
                if (result.IsNotNull()) yield return result;
            }
        }

        public override Type GetNestedType(string name, BindingFlags bindingAttr)
        {
            var result = m_wrapper.GetNestedType(name, bindingAttr);
            return OnResolveNestedType(result);
        }

        public override Type[] GetNestedTypes(global::System.Reflection.BindingFlags bindingAttr)
        {
            return InternalGetNestedTypes(bindingAttr).ToArray();
        }

        private IEnumerable<Type> InternalGetNestedTypes(BindingFlags bindingAttr)
        {
            foreach (var nested in m_wrapper.GetNestedTypes(bindingAttr))
            {
                var result = OnResolveNestedType(nested);
                if (result.IsNotNull()) yield return result;
            }
        }

        public override PropertyInfo[] GetProperties(BindingFlags bindingAttr)
        {
            return InternalGetProperties(bindingAttr).ToArray();
        }

        private IEnumerable<PropertyInfo> InternalGetProperties(BindingFlags bindingAttr)
        {
            foreach (var prop in m_wrapper.GetProperties(bindingAttr))
            {
                var result = OnResolveProperty(prop);
                if (result.IsNotNull()) yield return result;
            }
        }

        protected override PropertyInfo GetPropertyImpl(string name, BindingFlags bindingAttr, Binder binder, Type returnType, Type[] types, ParameterModifier[] modifiers)
        {
            var result = m_wrapper.GetProperty(name, bindingAttr, binder, returnType, types, modifiers);
            return OnResolveProperty(result);
        }

        protected override bool HasElementTypeImpl()
        {
            return m_wrapper.HasElementType;
        }

        public override object InvokeMember(string name, BindingFlags invokeAttr, Binder binder, object target, object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] namedParameters)
        {
            return m_wrapper.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
        }

        protected override bool IsArrayImpl()
        {
            return m_wrapper.IsArray;
        }

        protected override bool IsByRefImpl()
        {
            return m_wrapper.IsByRef;
        }

        protected override bool IsCOMObjectImpl()
        {
            return m_wrapper.IsCOMObject;
        }

        protected override bool IsPointerImpl()
        {
            return m_wrapper.IsPointer;
        }

        protected override bool IsPrimitiveImpl()
        {
            return m_wrapper.IsPrimitive;
        }

        public override global::System.Reflection.Module Module
        {
            get { return m_wrapper.Module; }
        }

        public override string Namespace
        {
            get { return m_wrapper.Namespace; }
        }

        public override Type UnderlyingSystemType
        {
            get { return m_wrapper.UnderlyingSystemType; }
        }

        public override object[] GetCustomAttributes(Type attributeType, bool inherit)
        {
            return m_wrapper.GetCustomAttributes(attributeType, inherit);
        }

        public override object[] GetCustomAttributes(bool inherit)
        {
            return m_wrapper.GetCustomAttributes(inherit);
        }

        public override bool IsDefined(Type attributeType, bool inherit)
        {
            return m_wrapper.IsDefined(attributeType, inherit);
        }

        public override string Name
        {
            get { return m_wrapper.Name; }
        }

        #region IEnumerable<FieldInfo> Members

        IEnumerator<FieldInfo> IEnumerable<FieldInfo>.GetEnumerator()
        {
            return InternalGetFields(AnyMember).GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        global::System.Collections.IEnumerator global::System.Collections.IEnumerable.GetEnumerator()
        {
            return InternalGetMembers(AnyMember).GetEnumerator();
        }

        #endregion

        #region IEnumerable<PropertyInfo> Members

        IEnumerator<PropertyInfo> IEnumerable<PropertyInfo>.GetEnumerator()
        {
            return InternalGetProperties(AnyMember).GetEnumerator();
        }

        #endregion

        #region IEnumerable<EventInfo> Members

        IEnumerator<EventInfo> IEnumerable<EventInfo>.GetEnumerator()
        {
            return InternalGetEvents(AnyMember).GetEnumerator();
        }

        #endregion

        #region IEnumerable<Type> Members

        IEnumerator<Type> IEnumerable<Type>.GetEnumerator()
        {
            return InternalGetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic).
                GetEnumerator();
        }

        #endregion

        #region IEnumerable<MethodInfo> Members

        IEnumerator<MethodInfo> IEnumerable<MethodInfo>.GetEnumerator()
        {
            return InternalGetMethods(AnyMember).GetEnumerator();
        }

        #endregion

        #region IEnumerable<ConstructorInfo> Members

        IEnumerator<ConstructorInfo> IEnumerable<ConstructorInfo>.GetEnumerator()
        {
            return InternalGetConstructors(AnyMember).GetEnumerator();
        }

        #endregion

        /// <summary>
        /// Create instance of the type, which is represented by
        /// the current metadata
        /// </summary>
        /// <param name="args">Constructor arguments</param>
        /// <returns>An instance of the current type</returns>
        public static T CreateInstance(params object[] args)
        {
            var targetType = typeof(T);
            return (T)targetType.InvokeMember(".ctor", BindingFlags.CreateInstance |
                BindingFlags.Public |
                BindingFlags.NonPublic |
                BindingFlags.Instance, null, targetType, args);
        }

        /// <summary>
        /// Get instance of the current class
        /// </summary>
        public static Type<T> Instance
        {
            get { return new Type<T>(); }
        }

        /// <summary>
        /// Get runtime representation of the specified type
        /// </summary>
        public static RuntimeTypeInfo RuntimeInfo
        {
            get { return m_runtimeInfo; }
        }

        /// <summary>
        /// Get default value of th specified object
        /// </summary>
        public static T Default
        {
            get { return NullRef<T>.Value; }
        }

        public override bool Equals(object o)
        {
            if (o.IsNull()) return false;
            var otherType = o.SafeCast<Type>();
            if (otherType.IsNull()) return false;
            return m_wrapper.Equals(otherType);
        }

        #region IEquatable<Type> Members

        bool IEquatable<Type>.Equals(Type other)
        {
            return Equals(other);
        }

        #endregion
    }
}
