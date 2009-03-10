using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    [Serializable]
    public enum ByteOrder: byte
    {
        /// <summary>
        /// Specify undefined/unknown byte order
        /// </summary>
        Undefined = 0,

        /// <summary>
        /// "Little-endian" means the most significant byte is on the right end of a word.
        /// </summary>
        LittleEndian,

        /// <summary>
        /// "Big-endian" means the most significant byte is on the left end of a word.
        /// </summary>
        BigEndian,
    }
}
