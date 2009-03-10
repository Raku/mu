using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Extensions
{
    using NumberFormatInfo = global::System.Globalization.NumberFormatInfo;

    /// <summary>
    /// Represents extension methods for any array types
    /// </summary>
    [TypeExtender(typeof(Array))]
    public static class ArrayExtensions
    {
        /// <summary>
        /// Adds a specified value to array elements
        /// </summary>
        /// <param name="array">Target array</param>
        /// <param name="value">Value to add</param>
        public static void Add<T>(this T[] array, T value)
            where T: struct, IConvertible, IComparable<T>, IFormattable, IEquatable<T>
        {
            array.Process(t =>
                {
                    object result = t;
                    switch (t.GetTypeCode())
                    {
                        case TypeCode.Byte:
                            result = t.ToByte() + value.ToByte(); break;
                        case TypeCode.Char:
                        case TypeCode.Int16:
                            result = t.ToInt16() + value.ToInt16(); break;
                        case TypeCode.Decimal:
                            result = t.ToDecimal() + value.ToDecimal(); break;
                        case TypeCode.Double:
                            result = t.ToDouble() + value.ToDouble(); break;
                        case TypeCode.Int32:
                            result = t.ToInt32() + value.ToInt32(); break;
                        case TypeCode.Int64:
                            result = t.ToInt64() + value.ToInt64(); break;
                        case TypeCode.SByte:
                            result = t.ToSByte() + value.ToSByte(); break;
                        case TypeCode.Single:
                            result = t.ToSingle() + value.ToSingle(); break;
                        case TypeCode.String:
                            result = t.ToString() + value.ToString(); break;
                        case TypeCode.UInt16:
                            result = t.ToUInt16() + value.ToUInt16(); break;
                        case TypeCode.UInt32:
                            result = t.ToUInt32() + value.ToUInt32(); break;
                        case TypeCode.UInt64:
                            result = t.ToUInt64() + value.ToUInt64(); break;
                    }
                    return result.UnsafeCast<T>();
                });
        }

        /// <summary>
        /// Performs an array processing
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="array"></param>
        /// <param name="processor"></param>
        public static void Process<T>(this T[] array, Converter<T, T> processor)
            where T : struct, IConvertible, IComparable<T>, IFormattable, IEquatable<T>
        {
            for (var i = 0; i < array.Length; i++)
                array[i] = processor(array[i]);
        }

        /// <summary>
        /// Subtracts a specified value to array elements
        /// </summary>
        /// <param name="array">Target array</param>
        /// <param name="value">Value to subtract</param>
        public static void Subtract<T>(this T[] array, T value)
            where T : struct, IConvertible, IComparable<T>, IFormattable, IEquatable<T>
        {
            array.Process(t =>
            {
                object result = t;
                switch (t.GetTypeCode())
                {
                    case TypeCode.Byte:
                        result = t.ToByte() - value.ToByte(); break;
                    case TypeCode.Char:
                    case TypeCode.Int16:
                        result = t.ToInt16() - value.ToInt16(); break;
                    case TypeCode.Decimal:
                        result = t.ToDecimal() - value.ToDecimal(); break;
                    case TypeCode.Double:
                        result = t.ToDouble() - value.ToDouble(); break;
                    case TypeCode.Int32:
                        result = t.ToInt32() - value.ToInt32(); break;
                    case TypeCode.Int64:
                        result = t.ToInt64() - value.ToInt64(); break;
                    case TypeCode.SByte:
                        result = t.ToSByte() - value.ToSByte(); break;
                    case TypeCode.Single:
                        result = t.ToSingle() - value.ToSingle(); break;
                    case TypeCode.String:
                        result = t.ToString().Delete(value.ToString()); break;
                    case TypeCode.UInt16:
                        result = t.ToUInt16() - value.ToUInt16(); break;
                    case TypeCode.UInt32:
                        result = t.ToUInt32() - value.ToUInt32(); break;
                    case TypeCode.UInt64:
                        result = t.ToUInt64() - value.ToUInt64(); break;
                }
                return result.UnsafeCast<T>();
            });
        }

        /// <summary>
        /// Multiply a specified value to array elements
        /// </summary>
        /// <param name="array">Target array</param>
        /// <param name="value">Value to multiply</param>
        public static void Multiply<T>(this T[] array, T value)
            where T : struct, IConvertible, IComparable<T>, IFormattable, IEquatable<T>
        {
            array.Process(t =>
            {
                object result = t;
                switch (t.GetTypeCode())
                {
                    case TypeCode.Byte:
                        result = t.ToByte() * value.ToByte(); break;
                    case TypeCode.Char:
                    case TypeCode.Int16:
                        result = t.ToInt16() * value.ToInt16(); break;
                    case TypeCode.Decimal:
                        result = t.ToDecimal() * value.ToDecimal(); break;
                    case TypeCode.Double:
                        result = t.ToDouble() * value.ToDouble(); break;
                    case TypeCode.Int32:
                        result = t.ToInt32() * value.ToInt32(); break;
                    case TypeCode.Int64:
                        result = t.ToInt64() * value.ToInt64(); break;
                    case TypeCode.SByte:
                        result = t.ToSByte() * value.ToSByte(); break;
                    case TypeCode.Single:
                        result = t.ToSingle() * value.ToSingle(); break;
                    case TypeCode.UInt16:
                        result = t.ToUInt16() * value.ToUInt16(); break;
                    case TypeCode.UInt32:
                        result = t.ToUInt32() * value.ToUInt32(); break;
                    case TypeCode.UInt64:
                        result = t.ToUInt64() * value.ToUInt64(); break;
                }
                return result.UnsafeCast<T>();
            });
        }

        /// <summary>
        /// Returns value indicating that the current array is null reference
        /// or doesn't contain any element
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static bool IsNullOrEmpty(this Array target)
        {
            return target.IsNull() || target.Length.IsZero();
        }

        /// <summary>
        /// Converts multi-dimensional array into array with sigle dimension
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static T[] ToSingleDimensional<T>(this Array target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var length = 0;
            //Calculate total length L = L1 + L2 +...+LN
            var result = new T[length];
            //TODO: Complete
            return result;
        }

        /// <summary>
        /// Converts the current bit array into unsigned byte
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Unsigned byte, which is represents by bit array</returns>
        public static byte ToByte(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(byte) * 8; //Get size of result, in bits
            var result = default(byte);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    byte flag = (byte)bits[i];
                    result |= (byte)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into signed byte
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>signed byte, which is represents by bit array</returns>
        public static sbyte ToSByte(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(sbyte) * 8; //Get size of result, in bits
            var result = default(sbyte);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    sbyte flag = (sbyte)bits[i];
                    result |= (sbyte)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into signed two-byte value
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Signed two-byte value, which is represents by bit array</returns>
        public static short ToInt16(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(short) * 8; //Get size of result, in bits
            var result = default(short);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    short flag = (byte)bits[i];
                    result |= (short)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into unsigned two-byte value
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Unsigned two-byte value, which is represents by bit array</returns>
        public static ushort ToUInt16(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(ushort) * 8; //Get size of result, in bits
            var result = default(ushort);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    ushort flag = (byte)bits[i];
                    result |= (ushort)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into signed 4-byte value
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Signed 4-byte value, which is represents by bit array</returns>
        public static int ToInt32(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(int) * 8; //Get size of result, in bits
            var result = default(int);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    int flag = (byte)bits[i];
                    result |= (int)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into unsigned 4-byte value
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Unsigned 4-byte value, which is represents by bit array</returns>
        public static uint ToUInt32(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(uint) * 8; //Get size of result, in bits
            var result = default(uint);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    uint flag = (byte)bits[i];
                    result |= (uint)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into signed 8-byte value
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Signed 8-byte value, which is represents by bit array</returns>
        public static long ToInt64(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(long) * 8; //Get size of result, in bits
            var result = default(long);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    long flag = (byte)bits[i];
                    result |= (long)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Converts the current bit array into unsigned 8-byte value
        /// </summary>
        /// <param name="bits">Array of bits</param>
        /// <returns>Unsigned 8-byte value, which is represents by bit array</returns>
        public static ulong ToUInt64(this Bit[] bits)
        {
            ExceptionManager.CheckOnNull(bits, "bits");
            const byte byteSize = sizeof(ulong) * 8; //Get size of result, in bits
            var result = default(ulong);
            for (var i = 0; i < Math.Min(byteSize, bits.Length); i++)
                unchecked
                {
                    ulong flag = (byte)bits[i];
                    result |= (ulong)(flag << i);
                }
            return result;
        }

        /// <summary>
        /// Change element type of the current array
        /// </summary>
        /// <param name="elementType">A new element type</param>
        /// <param name="target">Target array, which elements type must be changed</param>
        /// <returns>A new array</returns>
        public static Array ChangeElementType(this Array target, Type elementType)
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(elementType, "elementType");
            var lengths = new int[target.Rank];
            //Obtain length of the each dimension
            for (var i = 0; i < target.Rank; i++)
                lengths[i] = target.GetLength(i);
            //Create result array
            var result = Array.CreateInstance(elementType, lengths);
            //Now calculates the absolute length
            var absoluteLength = 0;
            foreach (var length in lengths)
                absoluteLength += length;
            Array.Copy(target, result, absoluteLength);
            return result;
        }

        /// <summary>
        /// Assign specified value to all array elements
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="target"></param>
        /// <param name="value">Value to assign</param>
        public static void Initialize<T>(this T[] target, T value)
        {
            if (target.IsNull()) return;
            for (var i = 0; i < target.Length; i++)
                target[i] = value;
        }
    }
}
