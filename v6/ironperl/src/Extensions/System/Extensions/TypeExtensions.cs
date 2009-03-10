using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Extensions;

namespace System.Extensions
{
    /// <summary>
    /// Represents an extensions for the System.Type
    /// </summary>
    [TypeExtender(typeof(Type))]
    public static class TypeExtensions
    {
        internal const BindingFlags AnyMember = BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static;
        
        /// <summary>
        /// Create array, which element type is defined by the current type
        /// </summary>
        /// <param name="arrayType"></param>
        /// <param name="lengths"></param>
        /// <returns></returns>
        public static Array CreateArray(this Type arrayType, params int[] lengths)
        {
            ExceptionManager.CheckOnNull(arrayType, "arrayType");
            ExceptionManager.CheckOnNull(lengths, "lengths");
            //Validate the input arrayType
            if (!arrayType.IsArray)
                ExceptionManager.Throw<InvalidTypeException>(arrayType.MakeArrayType());
            return Array.CreateInstance(arrayType.GetElementType(), lengths);
        }

        public static T InvokeMethod<T>(this Type target, string methodName, object owner, params object[] args)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(methodName, "methodName");
            var method = target.GetMethod(methodName, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
            if (method.IsNull())
                ExceptionManager.Throw<MissingMethodException>(target.Name, methodName);
            return method.Invoke<T>(owner, args);
        }

        internal static IEnumerable<T> GetInstanceMembers<T>(this Type innerType, bool flatteHierarchy)
            where T : MemberInfo
        {
            return innerType.GetMembers<T>(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic, flatteHierarchy);
        }

        internal static IEnumerable<T> GetMembers<T>(this Type innerType, BindingFlags flags, bool flatteHierarchy)
            where T : MemberInfo
        {
            ExceptionManager.CheckOnNull(innerType, "innerType");
            foreach (var member in innerType.GetMembers(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                if (member.GetType().IsSubclassOf((typeof(T))))
                    yield return member as T;
            if (flatteHierarchy && innerType.BaseType.IsNotNull())
                foreach (var member in GetInstanceMembers<T>(innerType.BaseType, flatteHierarchy))
                    yield return member;
        }

        /// <summary>
        /// Get runtime representation of the specified type
        /// </summary>
        /// <param name="innerType">Type metadata, whcih runtime representation is obtained</param>
        /// <returns>Runtime representation of the target type</returns>
        public static RuntimeTypeInfo GetRuntimeType(this Type innerType)
        {
            return new RuntimeTypeInfo(innerType);
        }

        /// <summary>
        /// Get size of the specified type, in bytes
        /// </summary>
        /// <param name="target">Target type</param>
        /// <returns>Size of the type instance</returns>
        public static int GetTypeSize(this Type target)
        {
            const BindingFlags fieldFlags = BindingFlags.Public | 
                BindingFlags.NonPublic | BindingFlags.DeclaredOnly | BindingFlags.Instance;
            if (target.IsNull()) return 0;
            var result = 0;
            while (target.IsNotNull())
            {
                foreach (var field in target.GetFields(fieldFlags))
                    result += field.GetFieldSize();
                target = target.BaseType;
            }
            return result;
        }

        /// <summary>
        /// Get object factory for the specified class
        /// </summary>
        /// <param name="target">Target class</param>
        /// <returns>Object factory for the specified class</returns>
        public static IObjectFactory GetObjectFactory(this Type target)
        {
            var result = target.GetCustomAttribute<ObjectProviderAttribute>();
            return result.IsNull() ? null : result.GetObjectFactory();
        }

        public static bool ContainsMember(this Type target, MemberTypes memberType, string memberName)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(memberName, "memberName");
            switch (memberType)
            {
                case MemberTypes.Constructor:
                    return target.GetMembers<ConstructorInfo>(AnyMember, true).IsNotNull(); break;
                case MemberTypes.Event:
                    return target.GetMembers<EventInfo>(AnyMember, true).IsNotNull(); break;
                case MemberTypes.Field:
                    return target.GetMembers<FieldInfo>(AnyMember, true).IsNotNull(); break;
                case MemberTypes.Method:
                    return target.GetMembers<MethodInfo>(AnyMember, true).IsNotNull(); break;
                case MemberTypes.NestedType:
                    return target.GetMembers<Type>(AnyMember, true).IsNotNull(); break;
                case MemberTypes.Property:
                    return target.GetMembers<PropertyInfo>(AnyMember, true).IsNotNull(); break;
            }
            return false;
        }

        public static bool Equals<T>(this Type target)
        {
            return target.Equals(typeof(T));
        }

        /// <summary>
        /// Checks whether the current type  
        /// can be cast to the type 
        /// represented by the T generic argument
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="target"></param>
        public static bool CanCastTo<T>(this Type target)
        {
            return target.CanCastTo(typeof(T));
        }

        /// <summary>
        /// Checks whether the current type
        /// can be cast to the type 
        /// represented by the destination argument
        /// </summary>
        /// <param name="source"></param>
        /// <param name="destination"></param>
        public static bool CanCastTo(this Type source, Type destination)
        {
            ExceptionManager.CheckOnNull(source, "source");
            ExceptionManager.CheckOnNull(destination, "destination");
            if (source.IsSubclassOf(destination)) return true;
            if (source.TheSame(destination)) return true;
            foreach (var iface in source.GetInterfaces())
            {
                if (iface.TheSame(destination)) return true;
                if (iface.CanCastTo(destination)) return true;
            }
            return false;
        }

        /// <summary>
        /// Compare two types
        /// </summary>
        /// <param name="value"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        internal static bool TheSame(this Type value1, Type value2)
        {
            if (value1.IsNull())
                return value2.IsNull();
            if (value2.IsNull()) return false;
            return value1.Assembly.Equals(value2.Assembly) &&
                value1.Name == value2.Name &&
                value1.Namespace == value2.Namespace;
        }

        /// <summary>
        /// Get default value for the current instance
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static object DefaultValue(this Type target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            //Create default value provider
            var nullRef = typeof(NullRef<>);
            //Pass generic argument to it
            nullRef = nullRef.MakeGenericType(target);
            return nullRef.GetProperty("Value", target).GetValue(null, null);
        }

        public static MethodInfo GetMethod(this Type target, string methodName, BindingFlags flags, Type returnType, params Type[] parameters)
        {
            var queryResult = from method in target.GetMethods(flags)
                              where method.Name == methodName &&
                              method.ReturnType.Equals(returnType) &&
                              method.GetParameters().TheSame(parameters)
                              select method;
            return queryResult.FirstOrDefault();
        }

        private static bool TheSame(this ParameterInfo[] parameters1, Type[] parameters2)
        {
            if (parameters1.Length != parameters2.Length) return false;
            for(var i=0; i<parameters1.Length; i++)
            {
                var param1 = parameters1[i];
                var param2 = parameters2[i];
                if (!param1.ParameterType.Equals(param2)) return false;
            }
            return true;
        }

        /// <summary>
        /// Get overloaded methods with specified name
        /// </summary>
        /// <param name="target"></param>
        /// <param name="methodName"></param>
        /// <returns></returns>
        public static MethodInfo[] GetOverloads(this Type target, string methodName)
        {
            return target.GetOverloads(methodName, BindingFlags.Instance | BindingFlags.Public);
        }

        /// <summary>
        /// Get overloaded methods with specified name
        /// </summary>
        /// <param name="target"></param>
        /// <param name="flags"></param>
        /// <param name="methodName"></param>
        /// <returns></returns>
        public static MethodInfo[] GetOverloads(this Type target, string methodName, BindingFlags flags)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(methodName, "methodName");
            var queryResult = from m in target.GetMethods(flags)
                              where m.Name == methodName
                              select m;
            return queryResult.ToArray();
        }
    }
}
