using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    using Thread = global::System.Threading.Thread;

    /// <summary>
    /// Represents extension methods for System.IConvertible interface
    /// </summary>
    [TypeExtender(typeof(IConvertible))]
    public static class IConvertibleExtensions
    {
        /// <summary>
        /// Indicates that the current value is equal to true
        /// </summary>
        /// <param name="target">Target value</param>
        /// <returns>True, if the current value is true; otherwise, false</returns>
        public static bool IsTrue(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            return target.ToBoolean(Thread.CurrentThread.CurrentCulture);
        }

        /// <summary>
        /// Indicates that the current value is equal to false
        /// </summary>
        /// <param name="target">Target value</param>
        /// <returns>True, if the current value is false; otherwise, false</returns>
        public static bool IsFalse(this IConvertible target)
        {
            return !target.IsTrue();
        }

        /// <summary>
        /// Emulates the IF statement
        /// </summary>
        /// <param name="target">The current value</param>
        /// <param name="onTrue">Code block, which is executed if the current value is true</param>
        /// <param name="onFalse">Code block, which is executed if the current value is false</param>
        public static void If(this IConvertible target, CodeBlock onTrue, CodeBlock onFalse)
        {
            ExceptionManager.CheckOnNull(target, "target");
            switch (target.ToBoolean(Thread.CurrentThread.CurrentCulture))
            {
                case true: if (onTrue.IsNotNull()) onTrue(); break;
                case false: if (onFalse.IsNotNull()) onFalse(); break;
            }
        }

        /// <summary>
        /// Executed the specified code block if the current value is true
        /// </summary>
        /// <param name="target">Target value</param>
        /// <param name="onTrue">Code block to be executed</param>
        public static void IfTrue(this IConvertible target, CodeBlock onTrue)
        {
            target.If(onTrue, null);
        }

        /// <summary>
        /// Executed the specified code block if the current value is false
        /// </summary>
        /// <param name="target">Target value</param>
        /// <param name="onTrue">Code block to be executed</param>
        public static void IfFalse(this IConvertible target, CodeBlock onFalse)
        {
            target.If(null, onFalse);
        }


        /// <summary>
        /// Converts the value of this instance to an equivalent Boolean value using
        ///     the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>A Boolean value equivalent to the value of this instance.</returns>
        public static bool ToBoolean(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToBoolean(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 8-bit unsigned integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 8-bit unsigned integer equivalent to the value of this instance.</returns>
        public static byte ToByte(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToByte(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent Unicode character using
        ///     the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>A Unicode character equivalent to the value of this instance.</returns>
        public static char ToChar(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToChar(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent System.DateTime using
        ///     the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>A System.DateTime instance equivalent to the value of this instance.</returns>
        public static DateTime ToDateTime(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToDateTime(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent System.Decimal number
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>A System.Decimal number equivalent to the value of this instance.</returns>
        public static decimal ToDecimal(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToDecimal(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent double-precision floating-point
        ///     number using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>A double-precision floating-point number equivalent to the value of this
        ///     instance.</returns>
        public static double ToDouble(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToDouble(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 16-bit signed integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 16-bit signed integer equivalent to the value of this instance.</returns>
        public static short ToInt16(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToInt16(currentFormatter);
        }
 
        /// <summary>
        /// Converts the value of this instance to an equivalent 32-bit signed integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 32-bit signed integer equivalent to the value of this instance.</returns>
        public static int ToInt32(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToInt32(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 64-bit signed integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 64-bit signed integer equivalent to the value of this instance.</returns>
        public static long ToInt64(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToInt64(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 8-bit signed integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 8-bit signed integer equivalent to the value of this instance.</returns>
        public static sbyte ToSByte(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToSByte(currentFormatter);
        }
 
        /// <summary>
        ///  Converts the value of this instance to an equivalent single-precision floating-point
        ///     number using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>A single-precision floating-point number equivalent to the value of this
        ///     instance.</returns>
        public static float ToSingle(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToSingle(currentFormatter);
        }
 
        /// <summary>
        /// Converts the value of this instance to an System.Object of the specified
        ///     System.Type that has an equivalent value, using the specified culture-specific
        ///     formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <param name="conversionType">The System.Type to which the value of this instance is converted.</param>
        /// <returns> An System.Object instance of type conversionType whose value is equivalent
        ///     to the value of this instance.</returns>
        public static object ToType(this IConvertible target, Type conversionType)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToType(conversionType, currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 16-bit unsigned integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns> An 16-bit unsigned integer equivalent to the value of this instance.</returns>
        public static ushort ToUInt16(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToUInt16(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 32-bit unsigned integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 32-bit unsigned integer equivalent to the value of this instance.</returns>
        public static uint ToUInt32(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToUInt32(currentFormatter);
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 64-bit unsigned integer
        ///     using the specified culture-specific formatting information.
        /// </summary>
        /// <param name="target">Value to be converted</param>
        /// <returns>An 64-bit unsigned integer equivalent to the value of this instance.</returns>
        public static ulong ToUInt64(this IConvertible target)
        {
            ExceptionManager.CheckOnNull(target, "target");
            var currentFormatter = Thread.CurrentThread.CurrentCulture;
            return target.ToUInt64(currentFormatter);
        }
    }
}
