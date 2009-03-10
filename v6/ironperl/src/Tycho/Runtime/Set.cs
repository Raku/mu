using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class Set<T> : ISet<T> {
        HashSet<T> _set;

        public Set () {
            _set = new HashSet<T> ();
        }

        public Set (IEqualityComparer<T> comparer) {
            _set = new HashSet<T> (comparer);
        }

        public Set (IEnumerable<T> items) {
            _set = new HashSet<T> (items);
        }

        public Set (IEnumerable<T> items, IEqualityComparer<T> comparer) {
            _set = new HashSet<T> (items, comparer);
        }

        public bool Contains (T item) {
            return _set.Contains (item);
        }

        public void IntersectWith (IEnumerable<T> items) {
            _set.IntersectWith (items);
        }

        public void UnionWith (IEnumerable<T> items) {
            _set.UnionWith (items);
        }

        public void ExceptWith (IEnumerable<T> items) {
            _set.ExceptWith (items);
        }

        public bool IsProperSubsetOf (IEnumerable<T> items) {
            return _set.IsProperSubsetOf (items);
        }

        public bool IsSubsetOf (IEnumerable<T> items) {
            return _set.IsSubsetOf (items);
        }

        public bool IsProperSupersetOf (IEnumerable<T> items) {
            return _set.IsProperSupersetOf (items);
        }

        public bool IsSupersetOf (IEnumerable<T> items) {
            return _set.IsSupersetOf (items);
        }

        public bool Overlaps (IEnumerable<T> items) {
            return _set.Overlaps (items);
        }

        public bool Add (T item) {
            return _set.Add (item);
        }

        public void Clear () {
            _set.Clear ();
        }

        public void CopyTo (T [] array, int arrayIndex) {
            _set.CopyTo (array, arrayIndex);
        }

        public int Count {
            get { return _set.Count; }
        }

        public bool IsReadOnly {
            get { return ((ICollection<T>) _set).IsReadOnly; }
        }

        public bool Remove (T item) {
            return _set.Remove (item);
        }

        public IEnumerator<T> GetEnumerator () {
            return ((IEnumerable<T>) _set).GetEnumerator ();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator () {
            return ((System.Collections.IEnumerable) _set).GetEnumerator ();
        }
    }
}
