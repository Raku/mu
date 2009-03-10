using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class ListObject : InstanceObject, IEnumerable<AnyObject> {
        public List<AnyObject> Items { get; private set; }

        private ListObject (AnyObject prototype, List<AnyObject> list) : base (prototype) {
            Items = list;
        }

        public ListObject (AnyObject prototype) : this (prototype, new List<AnyObject> ()) {}

        public ListObject (AnyObject prototype, params AnyObject [] items) : this (prototype, new List<AnyObject> (items)) {
        }

        public ListObject (AnyObject prototype, IEnumerable<AnyObject> items)
            : this (prototype, new List<AnyObject> (items)) {}

        public override int Count {
            get {
                return Items.Count;
            }
        }

        public override AnyObject this [AnyObject index] {
            get {
                return Items [TranslateNegativeIndex ((int) index)];
            }
            set {
                Items [TranslateNegativeIndex ((int) index)] = value;
            }
        }

        private int TranslateNegativeIndex(int i) {
            if (i < 0) {
                return Items.Count + i;
            } else {
                return i;
            }
        }

        public override void Add (AnyObject a) {
            Items.Add (a);
        }

        public override bool Remove (AnyObject a) {
            return Items.Remove (a);
        }

        public override void RemoveIndex (AnyObject index) {
            Items.RemoveAt (TranslateNegativeIndex ((int) index));
        }

        public override bool Contains (AnyObject a) {
            return Items.Contains (a);
        }

        public override void Insert (int index, AnyObject item) {
            Items.Insert (TranslateNegativeIndex (index), item);
        }

        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            if (arguments.Length != 1) {
                return false;
            }

            AnyObject otherList = arguments [0];
            if (Items.Count == otherList.Count) {
                for (int n = 0; n < Items.Count; n++) {
                    if (!Items [n].Match (results, otherList [RuntimeModule.CreateInteger (n)])) {
                        return false;
                    }
                }

                return true;
            } else {
                return false;
            }
        }

        public override AnyObject ShallowCopy () {
            return new ListObject (Prototype, (IEnumerable<AnyObject>) Items);
        }

        public override AnyObject ActuallySerialize () {
            return SerializationModule.CreateStructure (Symbols.SerializationList, SerializationModule.CreateList (Items.Select (item => item.Serialize ())));
        }

        public override string ToString (HashSet<AnyObject> done) {
            if (ToStringNoRecurse (done)) {
                string r = "list [";
                bool first = true;

                foreach (AnyObject item in Items) {
                    if (!first) {
                        r += ", ";
                    }

                    r += item.ToString (done);

                    first = false;
                }

                return r + "]";
            } else {
                return "...";
            }
        }

        [TychoGetter2 ("list", "count")]
        public static int PrototypeGetCount (AnyObject self) {
            return self.Count;
        }

        [TychoMethod2 ("list", "index-get")]
        public static AnyObject PrototypeIndexGet (AnyObject self, AnyObject index) {
            return self [index];
        }

        [TychoMethod2 ("list", "index-set")]
        public static AnyObject PrototypeIndexSet (AnyObject self, AnyObject index, AnyObject value) {
            return self [index] = value;
        }

        [TychoMethod2 ("list", "add")]
        public static AnyObject PrototypeAdd (AnyObject self, AnyObject item) {
            self.Add (item);
            return item;
        }

        [TychoMethod2 ("list", "insert")]
        public static void PrototypeInsert (AnyObject self, int index, AnyObject item) {
            self.Insert (index, item);
        }

        [TychoMethod2 ("list", "remove")]
        public static bool PrototypeRemove (AnyObject self, AnyObject item) {
            return self.Remove (item);
        }

        [TychoMethod2 ("list", "remove-index")]
        public static void PrototypeRemoveIndex (AnyObject self, AnyObject index) {
            self.RemoveIndex (index);
        }

        [TychoMethod2 ("list", "contains")]
        public static bool PrototypeContains (AnyObject self, AnyObject item) {
            return self.Contains (item);
        }

        [TychoMethod2 ("list", "match")]
        public static bool PrototypeMatch (AnyObject self, AnyObject results, AnyObject arg) {
            if (self == RuntimeModule.List) {
                return arg is ListObject;
            } else {
                return self.Match (results, arg);
            }
        }

        [TychoMethod2 ("list", "each")]
        public static AnyObject PrototypeEach (ListObject self, AnyObject block) {
            AnyObject lastValue = RuntimeModule.Null;

            for (int n = 0; n < self.Count; n++) {
                lastValue = block.Invoke (self [RuntimeModule.CreateInteger (n)]);
            }

            return lastValue;
        }

        [TychoMethod2 ("list", "map")]
        public static AnyObject PrototypeMap (ListObject self, AnyObject block) {
            AnyObject newList = RuntimeModule.CreateList ();

            for (int n = 0; n < self.Count; n++) {
                newList.Add (block.Invoke (self [RuntimeModule.CreateInteger (n)]));
            }

            return newList;
        }

        [TychoMethod2 ("list", "sort")]
        public static AnyObject PrototypeSort (ListObject self) {
            List<AnyObject> sortedItems = new List<AnyObject> (self.Items);
            sortedItems.Sort ();
            return RuntimeModule.CreateList (sortedItems);
        }

        [TychoMethod2 ("list", "filter")]
        public static AnyObject PrototypeFilter (ListObject self, AnyObject block) {
            AnyObject newList = RuntimeModule.CreateList ();

            for (int n = 0; n < self.Count; n++) {
                AnyObject item = self [RuntimeModule.CreateInteger (n)];
                if ((bool) block.Invoke (item)) {
                    newList.Add (item);
                }
            }

            return newList;
        }

        [TychoMethod2 ("list", "fold-left")]
        public static AnyObject FoldLeft (ListObject self, AnyObject folder) {
            List<AnyObject> items = self.Items;

            bool firstItem = true;
            AnyObject left = RuntimeModule.Null;

            foreach (var item in items) {
                if (firstItem) {
                    left = item;
                    firstItem = false;
                } else {
                    left = folder.Invoke (left, item);
                }
            }

            return left;
        }

        [TychoMethod2 ("list", "fold-right")]
        public static AnyObject FoldRight (ListObject self, AnyObject folder) {
            List<AnyObject> items = self.Items;

            bool firstItem = true;
            AnyObject right = RuntimeModule.Null;

            for (int n = items.Count - 1; n >= 0; n--) {
                AnyObject item = items[n];

                if (firstItem) {
                    right = item;
                    firstItem = false;
                } else {
                    right = folder.Invoke (item, right);
                }
            }

            return right;
        }

        [TychoMethod2 ("list", "concatenate")]
        [TychoMethod2 ("list", "plus")]
        public static AnyObject PrototypeConcatenate (ListObject self, ListObject other) {
            return RuntimeModule.CreateList (self.Items.Concat (other.Items));
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> refs = new HashSet<AnyObject> (Items);
                refs.Add (RuntimeModule.List);

                return refs;
            }
        }

        [TychoGetter2 ("list", "properties")]
        public static AnyObject GetProperties (AnyObject self) {
            return RuntimeModule.Object.GetProperty (RuntimeModule.List, Symbols.RuntimeProperties);
        }

        [TychoGetter2 ("list", "methods")]
        public static AnyObject GetMethods (AnyObject self) {
            return RuntimeModule.Object.GetProperty (RuntimeModule.List, Symbols.RuntimeMethods);
        }

        #region IEnumerable<AnyObject> Members

        IEnumerator<AnyObject> IEnumerable<AnyObject>.GetEnumerator () {
            return Items.GetEnumerator ();
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator () {
            return Items.GetEnumerator ();
        }

        #endregion

        [TychoGetter2 ("list", "enumerator")]
        public static NativeObject<IEnumerator<AnyObject>> GetEnumerator (AnyObject self) {
            return new NativeObject<IEnumerator<AnyObject>> (((IEnumerable<AnyObject>) self).GetEnumerator ());
        }

        [TychoMethodSchema ("list", "new")]
        static AnyObject CreateNewSchema = new UnlimitedParametersSchemaObject (1);
        [TychoMethod ("list", "new")]
        static AnyObject CreateNew (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.CreateList (arguments);
        }

        [TychoMethod2 ("list", "to-set")]
        public static AnyObject ToSet (ListObject self) {
            return RuntimeModule.CreateSet (self.Items);
        }

        [TychoMethod2 ("list", "join")]
        public static AnyObject Join (AnyObject left, AnyObject right, AnyObject leftKey, AnyObject rightKey) {
            return RuntimeModule.CreateList (left.Join (right, l => leftKey.Invoke (l), r => rightKey.Invoke (r), (l, r) => RuntimeModule.CreateList (l, r)));
        }

        [TychoMethod2 ("list", "left-join")]
        public static AnyObject LeftJoin (AnyObject left, AnyObject right, AnyObject leftKey, AnyObject rightKey) {
            Dictionary<AnyObject, AnyObject> rightValues = new Dictionary<AnyObject, AnyObject> ();

            foreach (var item in right) {
                var key = rightKey.Invoke (item);
                if (!rightValues.ContainsKey (key)) {
                    rightValues.Add (key, item);
                }
            }

            AnyObject result = RuntimeModule.CreateList ();

            foreach (var item in left) {
                AnyObject rightItem;

                if (!rightValues.TryGetValue (leftKey.Invoke (item), out rightItem)) {
                    rightItem = RuntimeModule.Null;
                }

                result.Add (RuntimeModule.CreateList (item, rightItem));
            }

            return result;
        }

        [TychoMethod2 ("list", "reverse")]
        public static AnyObject Reverse (AnyObject self) {
            return RuntimeModule.CreateList (self.Reverse ());
        }

        [TychoMethod2 ("list", "zip")]
        public static AnyObject Zip (AnyObject left, AnyObject right) {
            IEnumerator<AnyObject> eleft = ((IEnumerable<AnyObject>) left).GetEnumerator ();
            IEnumerator<AnyObject> eright = ((IEnumerable<AnyObject>) right).GetEnumerator ();
            AnyObject zipped = RuntimeModule.CreateList ();

            while (eleft.MoveNext () && eright.MoveNext ()) {
                zipped.Add (RuntimeModule.CreateList (eleft.Current, eright.Current));
            }

            return zipped;
        }
    }
}
