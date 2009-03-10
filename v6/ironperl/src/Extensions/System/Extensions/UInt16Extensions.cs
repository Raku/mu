using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for System.UInt16 class
    /// </summary>
    [TypeExtender(typeof(UInt16))]
    public static class UInt16Extensions
    {
        /// <summary>
        /// Get bit representation of the current value
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static Bit[] Bits(this UInt16 target)
        {
            var flag = 0x01;
            var result = new Bit[sizeof(UInt16) * 8];
            for (var i = 0; i < result.Length; i++)
            {
                result[i] = (target & flag) > 0 ? Bit.One : Bit.Zero;
                flag <<= 1;
            }
            return result;
        }

        /// <summary>
        /// Convert the current value into bytes
        /// </summary>
        /// <param name="value">Value to convert</param>
        /// <param name="order">Byte order in which bytes must be placed</param>
        /// <returns>Bytes, which represent the current value</returns>
        public static byte[] GetBytes(this UInt16 value, ByteOrder order)
        {
            var result = BitConverter.GetBytes(value);
            if (order != RuntimeServices.CurrentByteOrder)
                Array.Reverse(result);
            return result;
        }

        /// <summary>
        /// Convert the current value into bytes
        /// </summary>
        /// <param name="value">Value to convert</param>
        /// <returns>Bytes, which represent the current value</returns>
        public static byte[] GetBytes(this UInt16 value)
        {
            return value.GetBytes(RuntimeServices.CurrentByteOrder);
        }

        /// <summary>
        /// Get byte in the specified position
        /// </summary>
        /// <param name="value">Target value</param>
        /// <param name="index">Byte position</param>
        /// <returns>Byte in the specified position</returns>
        public static byte GetByte(this UInt16 value, byte index)
        {
            if (index >= sizeof(UInt16))
                ExceptionManager.Throw<ArgumentOutOfRangeException>("index");
            index <<= 3;
            return (byte)(value >> index);
        }

        /// <summary>
        /// Get lo byte of the current value
        /// </summary>
        /// <param name="value">Target value</param>
        /// <returns>Lo byte of the current value</returns>
        public static byte LoByte(this UInt16 value)
        {
            return value.GetByte(0);
        }

        /// <summary>
        /// Get hi byte of the current value
        /// </summary>
        /// <param name="value">Target value</param>
        /// <returns>Hi byte of the current value</returns>
        public static byte HiByte(this UInt16 value)
        {
            return value.GetByte(sizeof(UInt16) - 1);
        }
    }
}
