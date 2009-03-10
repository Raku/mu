using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic.Extensions;
using System.Collections.Generic;

namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Object class
    /// </summary>
    [TypeExtender(typeof(object))]
    public static class ObjectExtensions
    {
        /// <summary>
        /// Indicates that the specified reference is a null reference
        /// </summary>
        /// <param name="value">Reference to be tested</param>
        /// <returns>true, if specified value is a null reference, otherwise false</returns>
        public static bool IsNull(this object value)
        {
            return RuntimeServices.IsNull(value);
        }

        /// <summary>
        /// Indicates that the specified reference is not a null reference
        /// </summary>
        /// <param name="value">Reference to be tested</param>
        /// <returns>true, if specified value is not a null reference, otherwise false</returns>
        public static bool IsNotNull(this object value)
        {
            return RuntimeServices.IsNotNull(value);
        }

        /// <summary>
        /// Cast specified object to another object
        /// </summary>
        /// <typeparam name="T">Type of the casting result</typeparam>
        /// <param name="value">Value to be casted</param>
        /// <returns>Casting result</returns>
        /// <exception cref="System.InvalidCastException">The exception that is thrown if value could not be casted to specified type</exception>
        public static T UnsafeCast<T>(this object value)
        {
            return value.IsNull() ? default(T) : (T)value;
        }

        /// <summary>
        /// Cast specified object to another object.
        /// This method takes into accout custom type-cast methods.
        /// </summary>
        /// <typeparam name="T">Type of the casting result</typeparam>
        /// <param name="value">Value to be casted</param>
        /// <returns>Casting result</returns>
        /// <exception cref="System.InvalidCastException">The exception that is thrown if value could not be casted to specified type</exception>
        public static T CastTo<T>(this object value)
        {
            return (T)value.ChangeType(typeof(T));
        }

        /// <summary>
        /// Performs Instance Projection on the current object
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns>Instance projection of the current object</returns>
        public static T ReflectOn<T, G>(this G value)
            where T : class, IProjection<G>
            where G : class
        {
            if (value.IsNull()) return NullRef<T>.Value;
            return new System.Runtime.Projection<T, G>(value).Convert();
        }

        /// <summary>
        /// Cast specified object to another object
        /// </summary>
        /// <typeparam name="T">Type of the casting result</typeparam>
        /// <param name="value">Value to be castd</param>
        /// <returns>Casting result or null if it it impossible</returns>
        public static T SafeCast<T>(this object value)
        {
            return value is T ? value.UnsafeCast<T>() : default(T);
        }

        /// <summary>
        /// Cast one object to another object
        /// </summary>
        /// <typeparam name="T">Type of the casting result</typeparam>
        /// <param name="value">Value to be casted</param>
        /// <returns>Casting result. Always not null</returns>
        public static T RawCast<T>(this object value)
            where T : class
        {
            var memorySlot = MemoryManager.Alloc<T>();
            if (value.IsNull()) return memorySlot;
            MergeObjects(value, memorySlot);
            return memorySlot;
        }

        /// <summary>
        /// Cast the current object to covariance class
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        /// <exception cref="System.ArgumentNullException">The current value is null</exception>
        ///<exception cref="System.InvalidOperationException">Type T is not a covariant type for the current object</exception>
        public static T Covariance<T>(this object value)
        {
            ExceptionManager.CheckOnNull(value, "value");
            //Make generic type definition for the covariant type
            var covariant = typeof(T);
            covariant = covariant.GetGenericTypeDefinition();
            //Make generic type definition for the original type
            var original = value.GetType();
            original = original.GetGenericTypeDefinition();
            if (!original.CanCastTo(covariant))
                ExceptionManager.Throw<InvalidOperationException>();
            //Make a result type
            covariant = original.MakeGenericType(typeof(T).GetGenericArguments());
            //Allocate managed memory for the covariant type
            var result = MemoryManager.Alloc(covariant);
            //Now copy all fields from source object to result
            MemoryManager.CopyTo(value, result);
            return result.UnsafeCast<T>();
        }

        private static void MergeObjects(object initial, object result)
        {
            var initialType = initial.GetType();
            var resultType = result.GetType();
            if (initialType.Attributes.HasFlags(TypeAttributes.SequentialLayout) &&
                resultType.Attributes.HasFlags(TypeAttributes.SequentialLayout))
                SequentialMerge(initialType, result);
            else
                LexicalMerge(initial, result);
        }

        private static void SequentialMerge(object initial, object result)
        {
            //Provides field-by-field merge
            const BindingFlags flags = BindingFlags.DeclaredOnly | 
                BindingFlags.Instance | 
                BindingFlags.NonPublic | 
                BindingFlags.Public;
            var initialType = initial.GetType();
            var resultType = result.GetType();
            while (initialType.IsNotNull() && initialType.IsNotNull())
            {
                var initialFields = initialType.GetFields(flags);
                var resultFields = resultType.GetFields(flags);
                for (var i = 0; i < Math.Min(initialFields.Length, resultFields.Length); i++)
                {
                    var field1 = initialFields[i];
                    var field2 = resultFields[i];
                    if (field1.FieldType.CanCastTo(field2.FieldType))
                        field2.SetValue(result, field1.GetValue(initial));
                }
                initialType = initialType.BaseType;
                resultType = resultType.BaseType;
            }
        }

        private static void LexicalMerge(object initial, object result)
        {
            //Provides merging by field signature
            const BindingFlags flags = BindingFlags.DeclaredOnly |
                BindingFlags.Instance |
                BindingFlags.NonPublic |
                BindingFlags.Public;
            var initialType = initial.GetType();
            var resultType = result.GetType();
            while (initialType.IsNotNull() && resultType.IsNotNull())
            {
                foreach (var field1 in initialType.GetFields(flags))
                {
                    var field2 = resultType.GetField(field1.Name, flags);
                    if (field2.IsNull()) continue;
                    if(field1.FieldType.CanCastTo(field2.FieldType))
                        field2.SetValue(result, field1.GetValue(initial));
                }
                initialType = initialType.BaseType;
                resultType = resultType.BaseType;
            }
        }

        /// <summary>
        /// Checks if an object is compatible with a given type.
        /// </summary>
        /// <typeparam name="T">Type for compatibility checking</typeparam>
        /// <param name="value">Value for testing</param>
        /// <returns>true, if specified object can be converted to the specified type</returns>
        public static bool InstanceOf<T>(this object value)
        {
            return value is T;
        }

        /// <summary>
        /// Get size of the current instance, in bytes
        /// </summary>
        /// <param name="value">Target instance</param>
        /// <returns>Size of the current instance, in bytes</returns>
        public static int GetSize(this object value)
        {
            if (value.IsNull()) return IntPtr.Size;
            return value.GetType().GetTypeSize();
        }

        /// <summary>
        /// Checks if the specified class has overrided operator
        /// </summary>
        /// <param name="value">Target value</param>
        /// <param name="operation">Required operator</param>
        /// <returns>Value indicating that specified operator is existed</returns>
        public static bool OperatorDefined(this object value, OperatorKind operation)
        {
            ExceptionManager.CheckOnNull(value, "value");
            var targetType = value.GetType();
            switch (Type.GetTypeCode(targetType))
            {
                case TypeCode.Boolean: return BooleanOperatorDefined(operation);
                case TypeCode.Byte:
                case TypeCode.Char:
                case TypeCode.Decimal:
                case TypeCode.Double:
                case TypeCode.Int16:
                case TypeCode.Int32:
                case TypeCode.Int64:
                case TypeCode.SByte:
                case TypeCode.Single:
                case TypeCode.UInt16:
                case TypeCode.UInt32:
                case TypeCode.UInt64:
                    return NumericOperatorDefined(operation);
                case TypeCode.String:
                    return operation.HasFlags(OperatorKind.Addition);
                case TypeCode.DBNull:
                    return false;
                case TypeCode.Empty: 
                    return false;
            }
            var methodName = operation.GetOperatorMethodName();
            var result = targetType.FindMembers(MemberTypes.Method,
                BindingFlags.Static,
                (m, f) => m.Name == methodName,
                null);
            return !result.IsNullOrEmpty();
        }

        private static bool BooleanOperatorDefined(OperatorKind operation)
        {
            return operation.OneOf(OperatorKind.BitwiseAnd,
                OperatorKind.BitwiseOr,
                OperatorKind.LogicalNot,
                OperatorKind.Equality,
                OperatorKind.Inequality);
        }

        private static bool NumericOperatorDefined(OperatorKind operation)
        {
            return !operation.OneOf(OperatorKind.LogicalNot, OperatorKind.False,
                OperatorKind.True);
        }

        private static MethodInfo GetOperatorMethod(this object value, OperatorKind op)
        {
            ExceptionManager.CheckOnNull(value, "value");
            var methodName = op.GetOperatorMethodName();
            if (methodName.IsNull()) return null;
            return value.GetType().GetMethod(methodName, BindingFlags.Public | BindingFlags.Static);
        }

        /// <summary>
        /// Invokes binary operator
        /// </summary>
        /// <typeparam name="TResult">Operator return type</typeparam>
        /// <typeparam name="TOperand1">First operand type</typeparam>
        /// <typeparam name="TOperand2">Second operand type</typeparam>
        /// <param name="op1">First operand</param>
        /// <param name="op2">Second operand</param>
        /// <param name="operation">Required operator</param>
        /// <returns>Result of the operator invocation</returns>
        public static TResult Operator<TResult, TOperand1, TOperand2>(this TOperand1 op1, TOperand2 op2, OperatorKind operation)
        {
            if (operation.IsBinary().IsFalse())
                ExceptionManager.Throw<ArgumentException>(System.Properties.Resources.String_ExpectedBinary, "operation");
            return op1.Operator<TResult>(operation, op1);
        }

        /// <summary>
        /// Invokes unary operator
        /// </summary>
        /// <typeparam name="TResult">Operator return type</typeparam>
        /// <typeparam name="TOperand">First operand type</typeparam>
        /// <param name="operand">First operand</param>
        /// <param name="operation">Required operator</param>
        /// <returns>Result of the operator invocation</returns>
        public static TResult Operator<TResult, TOperand>(this TOperand operand, OperatorKind operation)
        {
            if (operation.IsUnary().IsFalse())
                ExceptionManager.Throw<ArgumentException>(System.Properties.Resources.String_ExpectedUnary, "operation");
            return operand.Operator<TResult>(operation).UnsafeCast<TResult>();
        }

        /// <summary>
        /// Invokes operator
        /// </summary>
        /// <typeparam name="T">Operator return type</typeparam>
        /// <param name="operand">First operand</param>
        /// <param name="operation">Required operator</param>
        /// <param name="operands">Other operands</param>
        /// <returns>Result of the operator invocation</returns>
        public static T Operator<T>(this object operand, OperatorKind operation, params object[] operands)
        {
            //Checks input arguments
            ExceptionManager.CheckOnNull(operand, "operand");
            ExceptionManager.CheckOnNull(operands, "operands");
            //Validate operand count
            if (operation.IsBinary() && operands.Length < 1)
                ExceptionManager.Throw<ArgumentException>(System.Properties.Resources.String_InvalidOperandCount, "operands");
            //Detects operand type
            switch (Type.GetTypeCode(operand.GetType()))
            {
                case TypeCode.Double:
                case TypeCode.Decimal:
                case TypeCode.Boolean:
                case TypeCode.Byte:
                case TypeCode.Char:
                case TypeCode.Int16:
                case TypeCode.Int32:
                case TypeCode.Int64:
                case TypeCode.SByte:
                case TypeCode.Single:
                case TypeCode.UInt16:
                case TypeCode.UInt32:
                case TypeCode.UInt64:
                    //if operand is a primitive type then performs standard operator for it
                    if (operation.IsUnary())
                        return StandardUnaryOperator<T>(operand, operation);
                    if (operation.IsBinary())
                        return StandardBinaryOperator<T>(operand, operands[0], operation);
                    break;
                    //if operand is a string then perfoms only + operator for it
                case TypeCode.String:
                    return StringBinaryOperator(operand, operands[0], operation).UnsafeCast<T>();
            }
            //If operand is not a primitive type then get it operator method and invoke
            return operand.InvokeOperator<T>(operation, operands);
        }

        private static T InvokeOperator<T>(this object operand, OperatorKind operation, params object[] operands)
        {
            var arguments = new object[0];
            if (operation.IsUnary()) arguments = new[] { operand };
            if (operation.IsBinary()) arguments = new[] { operand, operands[0] };
            var methodName = operation.GetOperatorMethodName();
            return operand.GetType().InvokeMember(methodName,
                BindingFlags.InvokeMethod | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic,
                null,
                null,
                arguments).SafeCast<T>();
        }

        private static string StringBinaryOperator(object operand1, object operand2, OperatorKind operation)
        {
            if (operation.HasFlags(OperatorKind.Addition).And(operand2.IsNotNull()))
                operand1 = String.Concat(operand1, operand2);
            return operand1.ToString();
        }

        private static T StandardUnaryOperator<T>(object operand, OperatorKind operation)
        {
            var dynamicMethod = new DynamicMethod(operation.GetOperatorMethodName(), typeof(T), new[] { operand.GetType() });
            var msil = dynamicMethod.GetILGenerator();
            //ldarg.0
            msil.Emit(OpCodes.Ldarg_0);
            switch (operation)
            {
                case OperatorKind.UnaryNegation:
                    msil.Emit(OpCodes.Neg); break;
                case OperatorKind.Increment:
                    msil.Emit(OpCodes.Ldc_I4_1);
                    msil.Emit(OpCodes.Add); break;
                case OperatorKind.Decrement:
                    msil.Emit(OpCodes.Ldc_I4_1);
                    msil.Emit(OpCodes.Sub); break;
                case OperatorKind.OnesComplement:
                    msil.Emit(OpCodes.Not); break;
                case OperatorKind.LogicalNot:
                    msil.Emit(OpCodes.Ldc_I4_0);
                    msil.Emit(OpCodes.Ceq);
                    break;
            }
            msil.Emit(OpCodes.Ret);
            return dynamicMethod.Invoke(null, new[] { operand }).UnsafeCast<T>();
        }

        private static T StandardBinaryOperator<T>(object operand1, object operand2, OperatorKind operation)
        {
            //If specified operator is cast-operation then 
            //apply standard conversion rules without MSIL-generation
            if (operation.OneOf(OperatorKind.Implicit, OperatorKind.Explicit))
                return operand1.ChangeType(operand2 as Type).UnsafeCast<T>();
            var dynamicMethod = new DynamicMethod(operation.GetOperatorMethodName(), typeof(T), new[] { operand1.GetType(), operand2.GetType() });
            var msil = dynamicMethod.GetILGenerator();
            msil.Emit(OpCodes.Ldarg_0);
            msil.Emit(OpCodes.Ldarg_1);
            switch (operation)
            {
                case OperatorKind.Addition:
                    msil.Emit(OpCodes.Add); break;
                case OperatorKind.Subtraction:
                    msil.Emit(OpCodes.Sub); break;
                case OperatorKind.BitwiseOr:
                    msil.Emit(OpCodes.Or); break;
                case OperatorKind.BitwiseAnd:
                    msil.Emit(OpCodes.And); break;
                case OperatorKind.Inequality:
                    msil.Emit(OpCodes.Ceq);
                    msil.Emit(OpCodes.Ldc_I4_0);
                    msil.Emit(OpCodes.Ceq); break;
                case OperatorKind.Equality:
                    msil.Emit(OpCodes.Ceq); break;
                case OperatorKind.Multiply:
                    msil.Emit(OpCodes.Mul); break;
                case OperatorKind.Division:
                    msil.Emit(OpCodes.Div); break;
                case OperatorKind.GreaterThan:
                    msil.Emit(OpCodes.Cgt); break;
                case OperatorKind.LessThat:
                    msil.Emit(OpCodes.Clt); break;
                case OperatorKind.LessThanOrEqual:
                    msil.Emit(OpCodes.Clt);
                    msil.Emit(OpCodes.Ldc_I4_0); 
                    msil.Emit(OpCodes.Ceq);
                    break;
                case OperatorKind.GreaterThanOrEqual:
                    msil.Emit(OpCodes.Cgt);
                    msil.Emit(OpCodes.Ldc_I4_0);
                    msil.Emit(OpCodes.Ceq);
                    break;
            }
            msil.Emit(OpCodes.Ret);
            return dynamicMethod.Invoke(null, new[] { operand1, operand2 }).UnsafeCast<T>();
        }

        /// <summary>
        /// Get value of the specified property
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="target"></param>
        /// <param name="propertyName"></param>
        /// <param name="indexes"></param>
        /// <returns></returns>
        internal static T GetPropertyValue<T>(this object target, string propertyName, params object[] indexes)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(propertyName, "propertyName");
            var prop = target.GetType().GetMembers<PropertyInfo>(TypeExtensions.AnyMember, true).Find(t => t.Name == propertyName);
            if (prop == null) return default(T);
            return prop.GetValue(target, indexes).UnsafeCast<T>();
        }

        /// <summary>
        /// Get value of the specified field
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="target"></param>
        /// <param name="propertyName"></param>
        /// <param name="indexes"></param>
        /// <returns></returns>
        internal static T GetFieldValue<T>(this object target, string fieldName)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(fieldName, "fieldName");
            var fld = target.GetType().GetMembers<FieldInfo>(TypeExtensions.AnyMember, true).Find(t => t.Name == fieldName);
            if (fld.IsNull()) return default(T);
            return fld.GetValue(target).UnsafeCast<T>();
        }

        internal static void SetFieldValue(this object target, string fieldName, object value)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(fieldName, "fieldName");
            var fld = target.GetType().GetMembers<FieldInfo>(TypeExtensions.AnyMember, true).Find(t => t.Name == fieldName);
            if (fld.IsNull()) return;
            fld.SetValue(target, value);
        }

        internal static void SetPropertyValue(this object target, string propertyName, object value, params object[] indexes)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(propertyName, "propertyName");
            var prop = target.GetType().GetMembers<PropertyInfo>(TypeExtensions.AnyMember, true).Find(t => t.Name == propertyName);
            if (prop.IsNull()) return;
            prop.SetValue(target, value, indexes);
        }

        /// <summary>
        /// Returns value indicating that the current value is equal
        /// to one of the specified values
        /// </summary>
        /// <typeparam name="T">Type of the value to compare</typeparam>
        /// <param name="target">Target value</param>
        /// <param name="values">Values, which are used for comparing</param>
        /// <returns>True, the current value is equal
        /// to one of the specified values; otherwise, false</returns>
        /// <exception cref="System.ArgumentNullException">Values are not defined</exception>
        public static bool OneOf<T>(this T target, params T[] values)
        {
            ExceptionManager.CheckOnNull(values, "values");
            foreach (var item in values)
            {
                if (target.IsNull())
                    if (item.IsNull()) return true;
                    else continue;
                if (target.Equals(item)) return true;
            }
            return false;
        }

        public static void For<T>(this T target, Converter<T, bool> condition, Converter<T, T> iteration, Action<T> forBlock)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            ExceptionManager.CheckOnNull(iteration, "iteration");
            ExceptionManager.CheckOnNull(condition, "condition");
            ExceptionManager.CheckOnNull(forBlock, "forBlock");
            var currentValue = target;
            while (!condition(currentValue))
            {
                forBlock(currentValue);
                currentValue = iteration(currentValue);
            }
        }

        private static bool TryChangeType(this object obj, Type targetClass, out object result)
        {
            ExceptionManager.CheckOnNull(targetClass, "targetClass");
            result = targetClass.DefaultValue();
            //If object is null the return default value for specified type
            if (obj.IsNull()) return false;
            var objType = obj.GetType();
            //If the current object provides conversion
            //to simple type then uses IConvertible members
            if (obj is IConvertible && !Type.GetTypeCode(targetClass).OneOf(TypeCode.Object, TypeCode.Empty))
            {
                var convertible = obj.UnsafeCast<IConvertible>();
                switch (Type.GetTypeCode(targetClass))
                {
                    case TypeCode.Boolean: result = convertible.ToBoolean(); return true;
                    case TypeCode.Byte: result = convertible.ToByte(); return true;
                    case TypeCode.Char: result= convertible.ToChar(); return true;
                    case TypeCode.DateTime: result = convertible.ToDateTime(); return true;
                    case TypeCode.DBNull: result = DBNull.Value; return true;
                    case TypeCode.Decimal: result = convertible.ToDecimal(); return true;
                    case TypeCode.Double: result = convertible.ToDouble(); return true;
                    case TypeCode.Int16: result = convertible.ToInt16(); return true;
                    case TypeCode.Int32: result = convertible.ToInt32(); return true;
                    case TypeCode.Int64: result = convertible.ToInt64(); return true;
                    case TypeCode.SByte: result = convertible.ToSByte(); return true;
                    case TypeCode.Single: result = convertible.ToSingle(); return true;
                    case TypeCode.String: result = convertible.ToString(); return true;
                    case TypeCode.UInt16: result = convertible.ToUInt16(); return true;
                    case TypeCode.UInt32: result = convertible.ToUInt32(); return true;
                    case TypeCode.UInt64: result = convertible.ToUInt64(); return true;
                }
            }
            //Perform reduced-cast
            if (objType.CanCastTo(targetClass)) { result = obj; return true; }
            //Define bindig flags for static method resolving
            const BindingFlags flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;
            //If obj contains conversion method then calls it
            foreach (var methodName in RuntimeServices.CastMethodNames)
            {
                var castMethod = objType.GetMethod(methodName, flags, targetClass, objType);
                if (castMethod.IsNotNull() && castMethod.ReturnType.CanCastTo(targetClass))
                { result = castMethod.Invoke(null, obj.AsArray()); return true; }
                castMethod = targetClass.GetMethod(methodName, flags, targetClass, objType);
                if (castMethod.IsNotNull())
                { result = castMethod.Invoke(null, obj.AsArray()); return true; }
            }
            //
            return false;
        }

        /// <summary>
        /// Late-bind typecast
        /// </summary>
        /// <param name="targetClass"></param>
        /// <returns></returns>
        public static object ChangeType(this object obj, Type targetClass)
        {
            var result = default(object);
            if (!obj.TryChangeType(targetClass, out result))
                throw new InvalidCastException();
            return result;
        }

        /// <summary>
        /// Late-bind typecast
        /// </summary>
        /// <param name="targetClass"></param>
        /// <returns></returns>
        public static object ChangeTypeAny(this object obj, Type targetClass)
        {
            var result = default(object);
            obj.TryChangeType(targetClass, out result);
            return result;
        }

        /// <summary>
        /// Converts the current value into array with single element
        /// </summary>
        /// <typeparam name="T">Type of the array</typeparam>
        /// <param name="value"></param>
        /// <returns>Array with single element</returns>
        public static T[] AsArray<T>(this T value)
        {
            return new[] { value };
        }

        /// <summary>
        /// Create clone of the current object
        /// </summary>
        /// <typeparam name="T">Clone type</typeparam>
        /// <param name="value">Value to be cloned</param>
        /// <param name="copyType">Kind of the cloning mechanism</param>
        /// <returns>Object clone</returns>
        public static T Clone<T>(this T value, ObjectCopy copyType)
        {
            switch (copyType)
            {
                case ObjectCopy.Shallow: return CreateShallowCopy(value);
                case ObjectCopy.Default: return CreateDefaultCopy(value);
                case ObjectCopy.Deep: return CreateDeepCopy(value);
            }
            throw new NotImplementedException();
        }

        private static T CreateDefaultCopy<T>(T value)
        {
            if (value is ICloneable)
            {
                var cloneable = value.UnsafeCast<ICloneable>();
                return cloneable.SafeCast<T>();
            }
            return CreateShallowCopy(value);
        }

        private static T CreateShallowCopy<T>(T value)
        {
            if (value.IsNull()) return NullRef<T>.Value;
            const BindingFlags methodFlags = BindingFlags.NonPublic | BindingFlags.Instance;
            var cloneMethod = value.GetType().GetMethod("MemberwiseClone", methodFlags, typeof(object), new Type[0]);
            return cloneMethod.Invoke(value, null).SafeCast<T>();
        }

        private static T CreateDeepCopy<T>(T value)
        {
            var cache = new Dictionary<int, object>();
            value = CreateDeepCopy(value, cache);
            //Destroy cache explicitly
            MemoryManager.Destroy(ref cache);
            return value;
        }

        private static T CreateDeepCopy<T>(T value, IDictionary<int, object> cache)
        {
            const BindingFlags fieldFlags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly;
            if (value.IsNull()) return NullRef<T>.Value;
            var typeInfo = value.GetType();
            var hashCode = value.GetHashCode();
            //Create shallow copy of the current value
            value = CreateShallowCopy(value);
            //Add this value to cache to avoid cyclic references
            cache.Add(hashCode, value);
            while (typeInfo.IsNotNull())
            {
                foreach (var field in typeInfo.GetFields(fieldFlags))
                {
                    //Get field value
                    var fieldValue = field.GetValue(value);
                    //Create deep copy of this value
                    if (fieldValue.IsNotNull())
                    {
                        hashCode = fieldValue.GetHashCode();
                        if (cache.ContainsKey(hashCode))
                            fieldValue = cache[hashCode];
                        else
                            if (Type.GetTypeCode(field.FieldType).OneOf(TypeCode.Object))
                                fieldValue = CreateDeepCopy(fieldValue, cache);
                    }
                    //Store deep copy of this value
                    field.SetValue(value, fieldValue);
                }
                typeInfo = typeInfo.BaseType;
            }
            return value;
        }

        /// <summary>
        /// Returns value indicating that the current value is a default
        /// value for it type
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns>Value indicating that the current value is a default
        /// value for it type</returns>
        public static bool IsDefault<T>(this T value)
        {
            var defValue = default(T);
            if (value.IsNull())
                return defValue.IsNull();
            return value.Equals(defValue);
        }

        /// <summary>
        /// Extract typed information about instance type
        /// </summary>
        /// <typeparam name="T">Type of the value</typeparam>
        /// <param name="value"></param>
        /// <returns>Typed information</returns>
        public static Type<T> GetClassInfo<T>(this T value)
        {
            return Type<T>.Instance;
        }

        /// <summary>
        /// Reduce object type to the one of the parent classes or implemented
        /// interfaces.
        /// </summary>
        /// <typeparam name="TObject">Type of the object.</typeparam>
        /// <typeparam name="TResult">Implemented interface or parent class.</typeparam>
        /// <param name="obj">Object to reduce.</param>
        /// <returns>Reduced object value.</returns>
        public static TResult Reduce<TObject, TResult>(this TObject obj)
            where TObject : TResult
        {
            return obj.UnsafeCast<TResult>();
        }
    }
}
