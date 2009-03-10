using System.Collections;
using System.Collections.Generic.Extensions;
using System.Extensions;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection.Extensions;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace System
{
    using BindingFlags = System.Reflection.BindingFlags;
    using FormatterServices = System.Runtime.Serialization.FormatterServices;
    using MethodBase = System.Reflection.MethodBase;
    using NotSupportedAttribute = System.Runtime.Compatibility.NotSupportedAttribute;

    /// <summary>
    /// Represents a runtime helpers
    /// </summary>
    public static class RuntimeServices
    {
        /// <summary>
        /// Indicates that the specified reference is a null reference
        /// </summary>
        /// <param name="value">Reference to be tested</param>
        /// <returns>true, if specified value is a null reference, otherwise false</returns>
        internal static bool IsNull(object value)
        {
            return value == null;
        }

        /// <summary>
        /// Indicates that the specified reference is not a null reference
        /// </summary>
        /// <param name="value">Reference to be tested</param>
        /// <returns>true, if specified value is not a null reference, otherwise false</returns>
        internal static bool IsNotNull(object value)
        {
            return !IsNull(value);
        }

        /// <summary>
        /// Validate a specified condition
        /// </summary>
        /// <param name="cond">Target condition</param>
        /// <param name="exceptionToThrow">Exception that will be throw if condition is false</param>
        /// <exception cref="System.ArgumentNullException">One of the input parameters is null</exception>
        public static void CheckCondition(Condition cond, Exception exceptionToThrow)
        {
            ExceptionManager.CheckOnNull(cond, "cond");
            ExceptionManager.CheckOnNull(exceptionToThrow, "exceptionToThrow");
            if (!cond()) throw exceptionToThrow;
        }

        /// <summary>
        /// Validate a specified condition
        /// </summary>
        /// <param name="cond"></param>
        /// <param name="exceptionType"></param>
        /// <param name="args"></param>
        public static void CheckCondition(Condition cond, Type exceptionType, params object[] args)
        {
            ExceptionManager.CheckOnNull(cond, "cond");
            ExceptionManager.CheckOnNull(exceptionType, "exceptionType");
            ExceptionManager.CheckOnNull(args, "args");
            if (!cond())
            {
                var exceptionInstance = Activator.CreateInstance(exceptionType, args) as Exception;
                if (IsNull(exceptionInstance)) ExceptionManager.Throw<InvalidTypeException>(typeof(Exception));
                CheckCondition(cond, exceptionInstance);
                
            }
        }

        /// <summary>
        /// Create specified array
        /// </summary>
        /// <typeparam name="T">An array type</typeparam>
        /// <param name="lengths">Dimensions lengths</param>
        /// <returns>Unitialized array</returns>
        public static T CreateArray<T>(params int[] lengths)
            where T : class, ICloneable, IList, ICollection, IEnumerable
        {
            return typeof(T).CreateArray(lengths) as T;
        }

        /// <summary>
        /// Get special method name for the specified operator
        /// </summary>
        /// <param name="op">Operator kind</param>
        /// <returns>Operator method name</returns>
        public static string GetOperatorMethodName(this OperatorKind op)
        {
            var fieldInfo = typeof(OperatorKind).GetField(op.ToString());
            if (fieldInfo.IsNull()) return null;
            var attr = fieldInfo.GetCustomAttribute<global::System.Extensions.OperatorMethodAttribute>();
            return attr.MethodName;
        }

        public static bool IsUnary(this OperatorKind op)
        {
            var fieldInfo = typeof(OperatorKind).GetField(op.ToString());
            if (fieldInfo.IsNull()) return false;
            var attr = fieldInfo.GetCustomAttribute<global::System.Extensions.OperatorMethodAttribute>();
            return attr.TypeOfOperator == System.Extensions.OperatorMethodAttribute.OperatorClass.Unary;
        }

        public static bool IsBinary(this OperatorKind op)
        {
            var fieldInfo = typeof(OperatorKind).GetField(op.ToString());
            if (fieldInfo.IsNull()) return false;
            var attr = fieldInfo.GetCustomAttribute<global::System.Extensions.OperatorMethodAttribute>();
            return attr.TypeOfOperator == System.Extensions.OperatorMethodAttribute.OperatorClass.Binary;
        }

        /// <summary>
        /// Indicates that an enumeration can be treated as a bit field; that is, a set of flags
        /// </summary>
        /// <typeparam name="T">Enumeration type</typeparam>
        /// <returns>Value indicating that an enumeration can be treated as a bit field</returns>
        public static bool IsFlagSet<T>()
            where T : struct, IComparable, IFormattable, IConvertible
        {
            var enumType = typeof(T);
            if (enumType.IsEnum.IsFalse())
                ExceptionManager.Throw<InvalidTypeException>(typeof(Enum));
            return enumType.IsDefined<FlagsAttribute>();
        }

        /// <summary>
        /// Get the byte order ("endianess") in which data is stored in this computer architecture.
        /// </summary>
        public static ByteOrder CurrentByteOrder
        {
            get
            {
                return BitConverter.IsLittleEndian ? ByteOrder.LittleEndian : ByteOrder.BigEndian;
            }
        }

        /// <summary>
        /// Get names of all cast operators
        /// </summary>
        internal static readonly string[] CastMethodNames = new[] { "op_Explicit", "op_Implicit" };

        /// <summary>
        /// Execute method with return value and suppress Virtual Method Table for it
        /// </summary>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static TResult SuppressVmtSlot<TResult>(Expression<Func<TResult>> func)
        {
            if (func.IsNull()) return default(TResult);
            var callTree = func.Body.SafeCast<MethodCallExpression>();
            if (callTree.IsNull()) return default(TResult);
            var novtable = CreateNoVTableStub(callTree);
            func = Expression.Lambda<Func<TResult>>(novtable, null);
            //Execute NO_VTABLE stub
            return func.Compile()();
        }

        /// <summary>
        /// Execute method without return value and suppress Virtual Method Table for it
        /// </summary>
        /// <param name="proc"></param>
        public static void SuppressVmtSlot(Expression<CodeBlock> proc)
        {
            if (proc.IsNull()) return;
            var callTree = proc.Body.SafeCast<MethodCallExpression>();
            if (callTree.IsNull()) return;
            var novtable = CreateNoVTableStub(callTree);
            proc = Expression.Lambda<CodeBlock>(novtable, null);
            //Execute NO_VTABLE stub
            proc.Compile()();
        }

        private static MethodCallExpression CreateNoVTableStub(MethodCallExpression expr)
        {
            //Resolve root method
            var novtable = expr.Method.GetBaseDefinition();
            //Create NO_VTABLE stub method
            var siganture = novtable.GetParameterTypes().AddToBegin(expr.Object.Type).ToArray();
            //Generate NO_VTABLE stub
            var dynamicMethod = new System.Reflection.Emit.DynamicMethod("NOVTABLE_STUB",
                novtable.ReturnType,
                siganture, novtable.Module);
            var ilgen = dynamicMethod.GetILGenerator();
            //emit first argument
            var parameters = dynamicMethod.GetParameters();
            ilgen.Emit(System.Reflection.Emit.OpCodes.Ldarg, 0);
            //Box first argument 
            if (parameters[0].ParameterType.IsValueType)
                ilgen.Emit(System.Reflection.Emit.OpCodes.Box, parameters[0].ParameterType);
            //Insert other arguments
            for (var i = 1; i < parameters.Length; i++)
                ilgen.Emit(System.Reflection.Emit.OpCodes.Ldarg, i);
            ilgen.Emit(System.Reflection.Emit.OpCodes.Call, novtable);
            ilgen.Emit(System.Reflection.Emit.OpCodes.Ret);
            return Expression.Call(expr.Object, dynamicMethod, expr.Arguments.AddToBegin(expr.Object));
        }

        /// <summary>
        /// Determines whether specified method is implemented.
        /// </summary>
        /// <param name="method">Target method.</param>
        /// <returns>True, if method contains an implementation; otherwise, false.</returns>
        public static bool IsSupported(this MethodBase method)
        {
            ExceptionManager.CheckOnNull(method, "method");
            return !method.IsDefined<NotSupportedAttribute>();
        }

        /// <summary>
        /// Determines whether currently executed methid is implemented and supported.
        /// </summary>
        /// <returns>True, if method contains an implementation; otherwise, false.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static bool IsSupported()
        {
            return IsSupported(StackWalk.GetCallerMethod());
        }

        /// <summary>
        /// Throws System.NotSupportedException exception if the 
        /// currently executed method is not implemented or supported.
        /// </summary>
        [DebuggerNonUserCode]
        [DebuggerHidden]
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static void ThrowIfNotSupported()
        {
            var caller = StackWalk.GetCallerMethod();
            var attr = caller.GetCustomAttribute<NotSupportedAttribute>();
            if (attr.IsNotNull())
                throw new NotSupportedException(attr.Reason);
        }

        /// <summary>
        /// Determines whether specified calling expression is supported.
        /// </summary>
        /// <param name="callInstruction">Calling expression to be validated.</param>
        /// <returns>True, if method contains an implementation; otherwise, false.</returns>
        /// <example>
        /// var supported = RuntimeServices.IsSupported(() => SomeMethod());
        /// </example>
        /// <exception cref="System.ArgumentException">Method call expression is required.</exception>
        public static bool IsSupported(Expression<CodeBlock> callInstruction)
        {
            ExceptionManager.CheckOnNull(callInstruction, "callInstruction");
            var method = default(MethodBase);
            if (callInstruction.Body.InstanceOf<MethodCallExpression>())
                method = callInstruction.Body.UnsafeCast<MethodCallExpression>().Method;
            if (callInstruction.Body is NewExpression)
                method = callInstruction.Body.UnsafeCast<NewExpression>().Constructor;
            if (method.IsNull())
                throw new ArgumentException(Properties.Resources.String_MethodCallExpected,
                    "callInstruction");
            return IsSupported(method);
        }
    }
}
