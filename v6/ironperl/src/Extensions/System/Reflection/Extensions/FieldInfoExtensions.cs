using System.Extensions;

namespace System.Reflection.Extensions
{
    using UnsafeAttribute = global::System.Runtime.CompilerServices.UnsafeAttribute;

    /// <summary>
    /// Represents extensions for the System.Reflection.FieldInfo class
    /// </summary>
    [TypeExtender(typeof(FieldInfo))]
    public static class FieldInfoExtensions
    {
        [Unsafe]
        internal unsafe static int GetFieldSize(this FieldInfo field)
        {
            ExceptionManager.CheckOnNull(field, "field");
            switch (Type.GetTypeCode(field.FieldType))
            {
                case TypeCode.Boolean: return sizeof(bool);
                case TypeCode.Byte: return sizeof(byte);
                case TypeCode.Char: return sizeof(char);
                case TypeCode.DateTime: return sizeof(DateTime);
                case TypeCode.String:
                case TypeCode.DBNull: return IntPtr.Size;
                case TypeCode.Decimal: return sizeof(decimal);
                case TypeCode.Double: return sizeof(double);
                case TypeCode.Int16: return sizeof(Int16);
                case TypeCode.Int32: return sizeof(Int32);
                case TypeCode.Int64: return sizeof(Int64);
                case TypeCode.SByte: return sizeof(SByte);
                case TypeCode.Single: return sizeof(Single);
                case TypeCode.UInt16: return sizeof(UInt16);
                case TypeCode.UInt32: return sizeof(UInt32);
                case TypeCode.UInt64: return sizeof(UInt64);
                default:
                    if (field.FieldType.IsValueType)
                        return field.FieldType.GetTypeSize();
                    if (field.FieldType.Equals(typeof(UIntPtr)))
                        return UIntPtr.Size;
                    return IntPtr.Size;
            }
        }

        /// <summary>
        /// Indicates that field can be overwritten
        /// </summary>
        /// <param name="field"></param>
        /// <returns>True, if field can be overwritten; otherwise, false</returns>
        public static bool CanWrite(this FieldInfo field)
        {
            ExceptionManager.CheckOnNull(field, "field");
            return field.Attributes.HasFlags(FieldAttributes.InitOnly).IsFalse();
        }
    }
}
