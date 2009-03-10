using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for System.UInt32 class
    /// </summary>
    [TypeExtender(typeof(UInt32))]
    public static class UInt32Extensions
    {
        /// <summary>
        /// Get bit representation of the current value
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static Bit[] Bits(this UInt32 target)
        {
            var flag = 0x01;
            var result = new Bit[sizeof(UInt32) * 8];
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
        public static byte[] GetBytes(this UInt32 value, ByteOrder order)
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
        public static byte[] GetBytes(this UInt32 value)
        {
            return value.GetBytes(RuntimeServices.CurrentByteOrder);
        }

        /// <summary>
        /// Get byte in the specified position
        /// </summary>
        /// <param name="value">Target value</param>
        /// <param name="index">Byte position</param>
        /// <returns>Byte in the specified position</returns>
        public static byte GetByte(this UInt32 value, byte index)
        {
            if (index >= sizeof(UInt32))
                ExceptionManager.Throw<ArgumentOutOfRangeException>("index");
            index <<= 3; //index *= 3;
            return (byte)(value >> index);
        }

        /// <summary>
        /// Get lo byte of the current value
        /// </summary>
        /// <param name="value">Target value</param>
        /// <returns>Lo byte of the current value</returns>
        public static byte LoByte(this UInt32 value)
        {
            return value.GetByte(0);
        }

        /// <summary>
        /// Get hi byte of the current value
        /// </summary>
        /// <param name="value">Target value</param>
        /// <returns>Hi byte of the current value</returns>
        public static byte HiByte(this UInt32 value)
        {
            return value.GetByte(sizeof(UInt32) - 1);
        }

        /// <summary>
        /// Convert the current value into words
        /// </summary>
        /// <param name="value">Value to convert</param>
        /// <returns>Words, which represent the current value</returns>
        public static ushort[] GetWords(this UInt32 value)
        {
            var result = new ushort[sizeof(UInt32) / sizeof(ushort)];
            for (byte i = 0; i < result.Length; i++)
                result[i] = value.GetWord(i);
            return result;
        }

        /// <summary>
        /// Get word in the specified position
        /// </summary>
        /// <param name="value">Target value</param>
        /// <param name="index">Word position</param>
        /// <returns>Word in the specified position</returns>
        public static ushort GetWord(this UInt32 value, byte index)
        {
            const int wordSize = sizeof(ushort) * 8; //Word size, in bits
            unchecked
            {
                return (ushort)(value >> (index * wordSize));
            }
        }

        /// <summary>
        /// Get lo word of the current value
        /// </summary>
        /// <param name="value">Target value</param>
        /// <returns>Lo word of the current value</returns>
        public static ushort LoWord(this UInt32 value)
        {
            return value.GetWord(0);
        }

        /// <summary>
        /// Get hi word of the current value
        /// </summary>
        /// <param name="value">Target value</param>
        /// <returns>Hi word of the current value</returns>
        public static ushort HiWord(this UInt32 value)
        {
            return value.GetWord(sizeof(UInt32) / sizeof(ushort) - 1);
        }
    }
}
