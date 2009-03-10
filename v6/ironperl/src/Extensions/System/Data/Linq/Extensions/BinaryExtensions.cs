using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Data.Linq.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Data.Linq.Binary class
    /// </summary>
    [TypeExtender(typeof(Binary))]
    public static class BinaryExtensions
    {
        /// <summary>
        /// Converts the current array in the SQL binary type
        /// </summary>
        /// <param name="byteArray">Array to convert</param>
        /// <returns>Array in the SQL-compliant form</returns>
        public static Binary ToLinqBinary(this byte[] byteArray)
        {
            return new Binary(byteArray);
        }
    }
}
