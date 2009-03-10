using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public interface ISet<T> : IEnumerable<T> {
        bool Add (T t);
        bool Remove (T t);
        void Clear ();
        void CopyTo (T [] array, int arrayIndex);
        bool IsReadOnly { get; }
        int Count { get; }
        bool Contains (T t);

        void IntersectWith (IEnumerable<T> items);
        void UnionWith (IEnumerable<T> items);
        void ExceptWith (IEnumerable<T> items);
        bool IsProperSubsetOf (IEnumerable<T> items);
        bool IsSubsetOf (IEnumerable<T> items);
        bool IsProperSupersetOf (IEnumerable<T> items);
        bool IsSupersetOf (IEnumerable<T> items);
        bool Overlaps (IEnumerable<T> items);
    }
}
