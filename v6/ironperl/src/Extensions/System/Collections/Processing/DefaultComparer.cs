using System.Collections.Generic;
using System.Extensions;

namespace System.Collections.Processing
{
    [Serializable]
    sealed class DefaultComparer<T> : IEqualityComparer<T>
    {
        #region IEqualityComparer<T> Members

        public bool Equals(T x, T y)
        {
            if (x.IsNull())
                if (y.IsNull()) return true;
                else return false;
            return x.Equals(y);
        }

        public int GetHashCode(T obj)
        {
            return obj.GetHashCode();
        }

        #endregion
    }
}
