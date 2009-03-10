using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Numeric
{
    /// <summary>
    /// Represents a complex number
    /// </summary>
    public struct Complex : IConvertible<Int32>
    {
        public int Image
        {
            get;
            set;
        }

        public int Real
        {
            get;
            set;
        }



        #region IConvertible<int> Members

        int IConvertible<int>.Convert()
        {
            return Real;
        }

        #endregion
    }
}
