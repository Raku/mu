using System.Extensions;

namespace System.IO.Extensions
{
    /// <summary>
    /// Represents extension methods for System.IO.BinaryWriter class
    /// </summary>
    [TypeExtender(typeof(BinaryWriter))]
    public static class BinaryWriterExtensions
    {
        /// <summary>
        /// Writes a two-byte signed integer to the current stream and advances the stream
        /// position by two bytes.
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value">The two-byte signed integer to write.</param>
        /// <param name="order">Byte order in which value must be stored in stream</param>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static void Write(this BinaryWriter writer, short value, ByteOrder order)
        {
            ExceptionManager.CheckOnNull(writer, "writer");
            writer.Write(value.GetBytes(order));
        }

        /// <summary>
        /// Writes a two-byte unsigned integer to the current stream and advances the stream
        /// position by two bytes.
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value">The two-byte unsigned integer to write.</param>
        /// <param name="order">Byte order in which value must be stored in stream</param>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static void Write(this BinaryWriter writer, ushort value, ByteOrder order)
        {
            ExceptionManager.CheckOnNull(writer, "writer");
            writer.Write(value.GetBytes(order));
        }

        /// <summary>
        ///  Writes a four-byte signed integer to the current stream and advances the
        ///  stream position by four bytes.
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value">The four-byte signed integer to write.</param>
        /// <param name="order">Byte order in which value must be stored in stream</param>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static void Write(this BinaryWriter writer, int value, ByteOrder order)
        {
            ExceptionManager.CheckOnNull(writer, "writer");
            writer.Write(value.GetBytes(order));
        }

        /// <summary>
        ///  Writes a four-byte unsigned integer to the current stream and advances the
        ///  stream position by four bytes.
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value">The four-byte unsigned integer to write.</param>
        /// <param name="order">Byte order in which value must be stored in stream</param>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static void Write(this BinaryWriter writer, uint value, ByteOrder order)
        {
            ExceptionManager.CheckOnNull(writer, "writer");
            writer.Write(value.GetBytes(order));
        }

        /// <summary>
        /// Writes an eight-byte signed integer to the current stream and advances the
        /// stream position by eight bytes.
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value">The eight-byte signed integer to write.</param>
        /// <param name="order">Byte order in which value must be stored in stream.</param>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static void Write(this BinaryWriter writer, long value, ByteOrder order)
        {
            ExceptionManager.CheckOnNull(writer, "writer");
            writer.Write(value.GetBytes(order));
        }

        /// <summary>
        /// Writes an eight-byte unsigned integer to the current stream and advances the
        /// stream position by eight bytes.
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value">The eight-byte unsigned integer to write.</param>
        /// <param name="order">Byte order in which value must be stored in stream.</param>
        /// <exception cref="System.IO.IOException">An I/O error occurs.</exception>
        /// <exception cref="System.ObjectDisposedException">The stream is closed.</exception>
        public static void Write(this BinaryWriter writer, ulong value, ByteOrder order)
        {
            ExceptionManager.CheckOnNull(writer, "writer");
            writer.Write(value.GetBytes(order));
        }
    }
}
