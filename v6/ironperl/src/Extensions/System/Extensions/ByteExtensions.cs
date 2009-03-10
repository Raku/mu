using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for System.Byte class
    /// </summary>
    [TypeExtender(typeof(byte))]
    public static class ByteExtensions
    {
        /// <summary>
        /// Get bit representation of the current value
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static Bit[] Bits(this byte target)
        {
            var flag = 0x01;
            var result = new Bit[sizeof(byte) * 8];
            for (var i = 0; i < result.Length; i++)
            {
                result[i] = (Bit)(byte)(target & flag);
                flag <<= 1;
            }
            return result;
        }
    }
}
