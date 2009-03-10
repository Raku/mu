using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;
using System.Runtime.InteropServices;
using System.Linq.Expressions;

namespace System
{
    using FormatterServices = System.Runtime.Serialization.FormatterServices;
    using BindingFlags = System.Reflection.BindingFlags;

    /// <summary>
    /// Represents managed memory managed
    /// </summary>
    public static class MemoryManager
    {
        /// <summary>
        /// Release a managed memory associated with the object
        /// </summary>
        /// <typeparam name="T">Object reference type</typeparam>
        /// <param name="value">Object reference</param>
        public static void Destroy<T>(ref T value)
            where T : class
        {
            Destroy<T>(ref value, false);
        }

        /// <summary>
        /// Release a managed memory associated with the object
        /// </summary>
        /// <typeparam name="T">Object reference type</typeparam>
        /// <param name="value">Object reference</param>
        /// <param name="suppressFinalizer">Indicates that the system not call the finalizer for the specified object</param>
        public static void Destroy<T>(ref T value, bool suppressFinalizer)
            where T : class
        {
            if (value.IsNull()) return;
            var generation = GC.GetGeneration(value);
            if (suppressFinalizer) GC.SuppressFinalize(value);
            Revoke(ref value);
            GC.Collect(generation);
            GC.WaitForPendingFinalizers();
        }

        /// <summary>
        /// Allocates required managed memory for specified type
        /// </summary>
        /// <typeparam name="T">Object type to be allocated</typeparam>
        /// <returns>Allocated object</returns>
        public static T Alloc<T>()
            where T : class
        {
            return Alloc(typeof(T)) as T;
        }

        /// <summary>
        /// Allocates required managed memory for specified type
        /// </summary>
        /// <param name="t">Object type to be allocated</param>
        /// <returns>Allocated object</returns>
        public static object Alloc(Type t)
        {
            if (t.IsNull()) return null;
            return FormatterServices.GetSafeUninitializedObject(t);
        }

        /// <summary>
        /// Initialize allocated managed memory with the new instance
        /// </summary>
        /// <param name="rawValue"></param>
        /// <param name="args"></param>
        public static void InitializeObject<T>(this T rawValue, params object[] args)
            where T : class
        {
            ExceptionManager.CheckOnNull(args, "args");
            ExceptionManager.CheckOnNull(rawValue, "rawValue");
            //Class constructor is just a method of an instance. See ECMA-335
            //We should to load unititialized instance onto stack and call constructor
            var ctor = ResolveConstructor<T>(args);
            if (ctor.IsNull())
                ExceptionManager.Throw<MissingMemberException>();
            //Initialize instance
            ctor.Invoke(rawValue, args);
        }

        private static System.Reflection.ConstructorInfo ResolveConstructor<T>(object[] args)
        {
            foreach (var ctor in typeof(T).GetConstructors(BindingFlags.Public | BindingFlags.Instance))
            {
                var parameters = ctor.GetParameters();
                //Count of arguments must be equal to count of constructor parameters
                if (parameters.Length != args.Length) continue;
                //Iterate through parameter for compatinilityt checking
                var matched = false;
                for (var i = 0; i < parameters.Length; i++)
                {
                    var argValue = args[i];
                    var param = parameters[i];
                    //Any null-reference compatible with classes
                    if (argValue.IsNull())
                        matched = !param.ParameterType.IsValueType;
                    else
                        if (param.ParameterType.IsByRef)
                            matched = param.ParameterType.GetElementType().IsAssignableFrom(argValue.GetType());
                        else
                            matched = param.ParameterType.IsAssignableFrom(argValue.GetType());
                    //If parameter doesn't match to argument then break
                    if (!matched) break;
                }
                if (matched) return ctor;
            }
            return null;
        }

        /// <summary>
        /// Copy fields from the current object to another object
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="source"></param>
        /// <param name="destination"></param>
        /// <exception cref="System.ArgumentNullException">One of the input arguments is null-reference</exception>
        public static void CopyTo<T>(T source, T destination)
        {
            ExceptionManager.CheckOnNull(source, "source");
            ExceptionManager.CheckOnNull(destination, "destination");
            const BindingFlags fieldFlags = BindingFlags.DeclaredOnly | BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
            var sourceType = source.GetType();
            var destType = destination.GetType();
            while (sourceType.IsNotNull() && destType.IsNotNull())
            {
                var sourceFields = sourceType.GetFields(fieldFlags);
                var destFields = destType.GetFields(fieldFlags);
                for (var i = 0; i < Math.Min(sourceFields.Length, destFields.Length); i++)
                {
                    var sourceValue = sourceFields[i].GetValue(source);
                    if (sourceValue is Array)
                        sourceValue = sourceValue.UnsafeCast<Array>().
                            ChangeElementType(destFields[i].FieldType.GetElementType());
                    destFields[i].SetValue(destination, sourceValue);
                }
                sourceType = sourceType.BaseType;
                destType = destType.BaseType;
            }
        }

        /// <summary>
        /// Nullify object reference
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        public static void Revoke<T>(ref T value)
            where T : class
        {
            value = null;
        }

        /// <summary>
        /// Allocates a block of unmanaged memory for specified structure
        /// and place structure into it
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value">Strcuture</param>
        /// <returns></returns>
        public static IntPtr AllocUnmanaged<T>(T value)
            where T : struct
        {
            var ptr = AllocUnmanaged<T>();
            Marshal.StructureToPtr(value, ptr, false);
            return ptr;
        }

        /// <summary>
        /// Allocates a block of unmanaged memory for specified structure
        /// and place structure into it
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static IntPtr AllocUnmanaged<T>()
            where T : struct
        {
            return Marshal.AllocHGlobal(Marshal.SizeOf(typeof(T)));
        }
    }
}
