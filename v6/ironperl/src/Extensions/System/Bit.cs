using System.Runtime.InteropServices;
using System.Extensions;

namespace System
{
    /// <summary>
    /// Represents a bit
    /// </summary>
    [Serializable]
    [StructLayout(LayoutKind.Sequential, Size = 1, Pack = 1, CharSet = CharSet.Unicode)]
    public struct Bit: IConvertible, IEquatable<Bit>, IComparable<Bit>, IFormattable
    {
        private readonly bool m_value;

        private Bit(bool initial)
        {
            m_value = initial;
            //var box=new Box<Bit>
        }

        /// <summary>
        /// Get a zero bit
        /// </summary>
        public static Bit Zero
        {
            get { return new Bit(false); }
        }

        /// <summary>
        /// Get a one bit
        /// </summary>
        public static Bit One
        {
            get { return new Bit(true); }
        }

        /// <summary>
        /// Get all possible bit values
        /// </summary>
        public static Bit[] Values
        {
            get { return new Bit[] { Zero, One }; }
        }

        /// <summary>
        /// Get string representation of the bit
        /// </summary>
        /// <returns>String representation of the bit</returns>
        public override string ToString()
        {
            return m_value ? "1" : "0";
        }

        public override bool Equals(object obj)
        {
            if (!obj.InstanceOf<Bit>()) return false;
            var otherBit = obj.UnsafeCast<Bit>();
            return otherBit.m_value == m_value;
        }

        public override int GetHashCode()
        {
            return m_value.GetHashCode();
        }

        #region IEquatable<Bit> Members

        bool IEquatable<Bit>.Equals(Bit other)
        {
            return Equals(other);
        }

        #endregion

        #region IComparable<Bit> Members

        public int CompareTo(Bit other)
        {
            if (other.m_value == m_value) return 0;
            if (m_value && !other.m_value) return 1;
            if (!m_value && other.m_value) return -1;
            throw new NotImplementedException();
        }

        #endregion

        public static explicit operator byte(Bit bit)
        {
            return bit.m_value ? (byte)1 : (byte)0;
        }

        public static explicit operator sbyte(Bit bit)
        {
            return bit.m_value ? (sbyte)1 : (sbyte)0;
        }

        public static explicit operator Bit(byte value)
        {
            return new Bit(value.ToBoolean());
        }

        #region IConvertible Members

        TypeCode IConvertible.GetTypeCode()
        {
            return TypeCode.Object;
        }

        bool IConvertible.ToBoolean(IFormatProvider provider)
        {
            return m_value;
        }

        byte IConvertible.ToByte(IFormatProvider provider)
        {
            return (byte)this;
        }

        char IConvertible.ToChar(IFormatProvider provider)
        {
            return m_value ? '1' : '0';
        }

        DateTime IConvertible.ToDateTime(IFormatProvider provider)
        {
            return new DateTime((byte)this);
        }

        decimal IConvertible.ToDecimal(IFormatProvider provider)
        {
            return (byte)this;
        }

        double IConvertible.ToDouble(IFormatProvider provider)
        {
            return double.MinValue;
        }

        short IConvertible.ToInt16(IFormatProvider provider)
        {
            return (byte)this;
        }

        int IConvertible.ToInt32(IFormatProvider provider)
        {
            return (byte)this;
        }

        long IConvertible.ToInt64(IFormatProvider provider)
        {
            return (byte)this;
        }

        sbyte IConvertible.ToSByte(IFormatProvider provider)
        {
            return (sbyte)this;
        }

        float IConvertible.ToSingle(IFormatProvider provider)
        {
            return float.MinValue;
        }

        string IConvertible.ToString(IFormatProvider provider)
        {
            return ToString();
        }

        object IConvertible.ToType(Type conversionType, IFormatProvider provider)
        {
            ExceptionManager.CheckOnNull(conversionType, "conversionType");
            switch (Type.GetTypeCode(conversionType))
            {
                case TypeCode.Boolean: return m_value;
                case TypeCode.Byte: return this.ToByte();
                case TypeCode.Char: return this.ToChar();
                case TypeCode.DateTime: return this.ToDateTime();
                case TypeCode.DBNull: return DBNull.Value;
                case TypeCode.Decimal: return this.ToDecimal();
                case TypeCode.Double: return this.ToDouble();
                case TypeCode.Int16: return this.ToInt16();
                case TypeCode.Int32: return this.ToInt32();
                case TypeCode.Int64: return this.ToInt64();
                case TypeCode.SByte: return this.ToSByte();
                case TypeCode.Object: return this;
                case TypeCode.Single: return this.ToSingle();
                case TypeCode.String: return ToString();
                case TypeCode.UInt16: return this.ToUInt16();
                case TypeCode.UInt32: return this.ToUInt32();
                case TypeCode.UInt64: return this.ToUInt64();
            }
            throw new NotSupportedException();
        }

        ushort IConvertible.ToUInt16(IFormatProvider provider)
        {
            return (byte)this;
        }

        uint IConvertible.ToUInt32(IFormatProvider provider)
        {
            return (byte)this;
        }

        ulong IConvertible.ToUInt64(IFormatProvider provider)
        {
            return (byte)this;
        }

        #endregion

        #region IFormattable Members

        string IFormattable.ToString(string format, IFormatProvider formatProvider)
        {
            return m_value.ToString(formatProvider);
        }

        #endregion

        public static Bit operator |(Bit op1, Bit op2)
        {
            return new Bit(op1.m_value | op2.m_value);
        }

        public static Bit operator &(Bit op1, Bit op2)
        {
            return new Bit(op1.m_value & op2.m_value);
        }

        public static Bit operator ~(Bit operand)
        {
            return new Bit(!operand.m_value);
        }

        public static Bit operator ^(Bit op1, Bit op2)
        {
            return new Bit(op1.m_value != op2.m_value);
        }
    }
}
