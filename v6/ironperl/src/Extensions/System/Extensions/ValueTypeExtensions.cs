namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.ValueType class
    /// </summary>
    [TypeExtender(typeof(ValueType))]
    public static class ValueTypeExtensions
    {
        /// <summary>
        /// Indicates that specified value is equal to zero
        /// </summary>
        /// <typeparam name="T">Target structure type</typeparam>
        /// <param name="target">Target structure</param>
        /// <returns>True, if the current values is zero; otherwise, false</returns>
        public static bool IsZero<T>(this T target)
            where T : struct, IConvertible
        {
            return target.ToInt64() == 0;
        }

        /// <summary>
        /// Indicates that specified value is not equal to zero
        /// </summary>
        /// <typeparam name="T">Target structure type</typeparam>
        /// <param name="target">Target structure</param>
        /// <returns>True, if the current values is not zero; otherwise, false</returns>
        public static bool IsNotZero<T>(this T target)
            where T : struct, IConvertible
        {
            return target.IsZero().IsFalse();
        }

        /// <summary>
        /// Returns value indicating that the current value has
        /// the specified flags
        /// </summary>
        /// <typeparam name="T">Type of the value</typeparam>
        /// <param name="target">Target value</param>
        /// <param name="values">Flags</param>
        /// <returns>True, the current value has the specified flags; otherwise, false</returns>
        public static bool HasFlags<T>(this T target, params IConvertible[] values)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            ExceptionManager.CheckOnNull(values, "values");
            if (values.Length.IsZero()) return true;
            foreach (var item in values)
                if (!target.HasFlag(item)) return false;
            return true;
        }

        private static bool HasFlag<T>(this T target, IConvertible value)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            if (value.IsNull()) return false; //May be true???
            switch (target.GetTypeCode())
            {
                case TypeCode.Byte:
                    return (target.ToByte() & value.ToByte()) != 0;
                case TypeCode.Int16:
                case TypeCode.Char:
                    return (target.ToInt16() & value.ToInt16()) != 0;
                case TypeCode.Int64:
                    return (target.ToInt64() & value.ToInt64()) != 0;
                case TypeCode.SByte:
                    return (target.ToSByte() & value.ToSByte()) != 0;
                case TypeCode.UInt16:
                    return (target.ToUInt16() & value.ToUInt16()) != 0;
                case TypeCode.UInt64:
                    return (target.ToUInt64() & value.ToUInt64()) != 0;
                case TypeCode.Int32:
                    return (target.ToInt32() & value.ToInt32()) != 0;
                case TypeCode.UInt32:
                    return (target.ToUInt32() & value.ToUInt32()) != 0;
                default:
                    ExceptionManager.Throw<InvalidOperationException>(System.Properties.Resources.String_InvalidPrimitive);
                    break;
            }
            return false;
        }

        /// <summary>
        /// Add specified flags to the current value
        /// </summary>
        /// <typeparam name="T">Type of the value</typeparam>
        /// <param name="target">Value to modify</param>
        /// <param name="values">Flags to be added</param>
        /// <returns>Modified value</returns>
        public static T AddFlags<T>(this T target, params IConvertible[] values)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            ExceptionManager.CheckOnNull(values, "values");
            foreach (var item in values)
                target = target.AddFlag(item);
            return target;
        }

        private static T AddFlag<T>(this T target, IConvertible value)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            if (value.IsNull()) return target;
            var result = default(object);
            switch (target.GetTypeCode())
            {
                case TypeCode.Byte:
                    result = target.ToByte() | value.ToByte(); break;
                case TypeCode.Int16:
                case TypeCode.Char:
                    result = target.ToInt16() | value.ToInt16(); break;
                case TypeCode.Int64:
                    result = target.ToInt64() | value.ToInt64(); break;
                case TypeCode.SByte:
                    result = target.ToSByte() | value.ToSByte(); break;
                case TypeCode.UInt16:
                    result = target.ToUInt16() | value.ToUInt16(); break;
                case TypeCode.UInt64:
                    result = target.ToUInt64() | value.ToUInt64(); break;
                case TypeCode.Int32:
                    result = target.ToInt32() | value.ToInt32(); break;
                case TypeCode.UInt32:
                    result = target.ToUInt32() | value.ToUInt32(); break;
                default:
                    ExceptionManager.Throw<InvalidOperationException>(System.Properties.Resources.String_InvalidPrimitive);
                    break;
            }
            return result.UnsafeCast<T>();
        }

        /// <summary>
        /// Excludes specified flags from the current value
        /// </summary>
        /// <typeparam name="T">Type of the value</typeparam>
        /// <param name="target">Value to modify</param>
        /// <param name="values">Flags to be excluded</param>
        /// <returns>Modified value</returns>
        public static T Exclude<T>(this T target, params IConvertible[] values)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            ExceptionManager.CheckOnNull(values, "values");
            foreach (var item in values)
                target = target.ExcludeOne(item);
            return target;
        }

        private static T ExcludeOne<T>(this T target, IConvertible value)
            where T : struct, IConvertible, IFormattable, IComparable
        {
            if (value.IsNull()) return target;
            var result = default(object);
            switch (target.GetTypeCode())
            {
                case TypeCode.Byte:
                    result = target.ToByte() & ~value.ToByte(); break;
                case TypeCode.Int16:
                case TypeCode.Char:
                    result = target.ToInt16() & ~value.ToInt16(); break;
                case TypeCode.Int64:
                    result = target.ToInt64() & ~value.ToInt64(); break;
                case TypeCode.SByte:
                    result = target.ToSByte() & ~value.ToSByte(); break;
                case TypeCode.UInt16:
                    result = target.ToUInt16() & ~value.ToUInt16(); break;
                case TypeCode.UInt64:
                    result = target.ToUInt16() & ~value.ToUInt16(); break;
                case TypeCode.Int32:
                    result = target.ToInt32() & ~value.ToInt32(); break;
                case TypeCode.UInt32:
                    result = target.ToUInt32() & ~value.ToUInt32(); break;
                default:
                    ExceptionManager.Throw<InvalidOperationException>(System.Properties.Resources.String_InvalidPrimitive);
                    break;
            }
            return result.UnsafeCast<T>();
        }

        /// <summary>
        /// Converts the current struct to the nullable value
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        public static T? AsNullable<T>(this T value)
            where T : struct
        {
            return new T?(value);
        }
    }
}
