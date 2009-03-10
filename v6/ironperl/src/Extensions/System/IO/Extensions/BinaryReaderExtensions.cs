using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.IO.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.BinaryReader class
    /// </summary>
    [TypeExtender(typeof(BinaryReader))]
    public static class BinaryReaderExtensions
    {
        /// <summary>
        /// Reads a 2-byte signed integer from the current stream and advances the current
        /// position of the stream by two bytes.
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="order">Byte order, in which value must be restored from stream.</param>
        /// <returns>A 2-byte signed integer read from the current stream.</returns>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static short ReadInt16(this BinaryReader reader, ByteOrder order)
        {
            var bytes = reader.ReadBytes(sizeof(short));
            if (RuntimeServices.CurrentByteOrder != order)
                Array.Reverse(bytes);
            return BitConverter.ToInt16(bytes, 0);
        }

        /// <summary>
        /// Reads a 2-byte unsigned integer from the current stream and advances the current
        /// position of the stream by two bytes.
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="order">Byte order, in which value must be restored from stream.</param>
        /// <returns>A 2-byte unsigned integer read from the current stream.</returns>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static ushort ReadUInt16(this BinaryReader reader, ByteOrder order)
        {
            var bytes = reader.ReadBytes(sizeof(ushort));
            if (RuntimeServices.CurrentByteOrder != order)
                Array.Reverse(bytes);
            return BitConverter.ToUInt16(bytes, 0);
        }

        /// <summary>
        /// Reads a 4-byte signed integer from the current stream and advances the current
        /// position of the stream by two bytes.
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="order">Byte order, in which value must be restored from stream.</param>
        /// <returns>A 4-byte signed integer read from the current stream.</returns>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static int ReadInt32(this BinaryReader reader, ByteOrder order)
        {
            var bytes = reader.ReadBytes(sizeof(int));
            if (RuntimeServices.CurrentByteOrder != order)
                Array.Reverse(bytes);
            return BitConverter.ToInt32(bytes, 0);
        }

        /// <summary>
        /// Reads a 4-byte unsigned integer from the current stream and advances the current
        /// position of the stream by two bytes.
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="order">Byte order, in which value must be restored from stream.</param>
        /// <returns>A 4-byte unsigned integer read from the current stream.</returns>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static uint ReadUInt32(this BinaryReader reader, ByteOrder order)
        {
            var bytes = reader.ReadBytes(sizeof(uint));
            if (RuntimeServices.CurrentByteOrder != order)
                Array.Reverse(bytes);
            return BitConverter.ToUInt32(bytes, 0);
        }

        /// <summary>
        /// Reads a 8-byte signed integer from the current stream and advances the current
        /// position of the stream by two bytes.
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="order">Byte order, in which value must be restored from stream.</param>
        /// <returns>A 8-byte signed integer read from the current stream.</returns>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static long ReadInt64(this BinaryReader reader, ByteOrder order)
        {
            var bytes = reader.ReadBytes(sizeof(long));
            if (RuntimeServices.CurrentByteOrder != order)
                Array.Reverse(bytes);
            return BitConverter.ToInt64(bytes, 0);
        }

        /// <summary>
        /// Reads a 8-byte unsigned integer from the current stream and advances the current
        /// position of the stream by two bytes.
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="order">Byte order, in which value must be restored from stream.</param>
        /// <returns>A 8-byte unsigned integer read from the current stream.</returns>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static ulong ReadUInt64(this BinaryReader reader, ByteOrder order)
        {
            var bytes = reader.ReadBytes(sizeof(ulong));
            if (RuntimeServices.CurrentByteOrder != order)
                Array.Reverse(bytes);
            return BitConverter.ToUInt64(bytes, 0);
        }
    }
}
