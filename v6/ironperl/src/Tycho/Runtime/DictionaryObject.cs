using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class DictionaryObject : InstanceObject, IEnumerable<AnyObject> {
        public Dictionary<AnyObject, AnyObject> Dictionary { get; private set; }

        public DictionaryObject (AnyObject prototype) : base (prototype) {
            Dictionary = new Dictionary<AnyObject, AnyObject> ();
        }

        public DictionaryObject (AnyObject prototype, params AnyObject [] entries) : this (prototype) {
            if (entries.Length % 2 != 0) throw new TychoException ("expect key/value pairs to create dictionary");

            for (int n = 0; n < entries.Length; n += 2) {
                Dictionary.Add (entries [n], entries [n + 1]);
            }
        }

        public override void Add (AnyObject key, AnyObject value) {
            Dictionary.Add (key, value);
        }

        public override bool ContainsKey (AnyObject key) {
            return Dictionary.ContainsKey (key);
        }

        public override bool ContainsValue (AnyObject value) {
            return Dictionary.ContainsValue (value);
        }

        public override AnyObject this [AnyObject index] {
            get {
                return Dictionary [index];
            }
            set {
                Dictionary [index] = value;
            }
        }

        public override int Count {
            get {
                return Dictionary.Count;
            }
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> refs = new HashSet<AnyObject> (Dictionary.Keys);
                refs.UnionWith (Dictionary.Values);
                return refs;
            }
        }

        public override AnyObject ShallowCopy () {
            DictionaryObject dict = new DictionaryObject (Prototype);

            foreach (KeyValuePair<AnyObject, AnyObject> item in Dictionary) {
                dict.Dictionary.Add (item.Key, item.Value);
            }

            return dict;
        }

        #region IEnumerable Members

        public IEnumerator<AnyObject> GetEnumerator () {
            return (from kv in Dictionary select RuntimeModule.CreateStructure (Symbols.RuntimeKey, kv.Key, Symbols.RuntimeValue, kv.Value)).GetEnumerator ();
        }

        #endregion

        [TychoMethod2 ("dictionary", "index-get")]
        public static AnyObject IndexGet (AnyObject self, AnyObject index) {
            return self [index];
        }

        [TychoMethod2 ("dictionary", "index-set")]
        public static AnyObject IndexSet (AnyObject self, AnyObject index, AnyObject value) {
            return self [index] = value;
        }

        [TychoGetter2 ("dictionary", "count")]
        public static int GetCount (AnyObject self) {
            return self.Count;
        }

        [TychoMethod2 ("dictionary", "index-set")]
        public static void Add (AnyObject self, AnyObject index, AnyObject value) {
            self.Add (index, value);
        }

        public bool TryGetValue (AnyObject index, out AnyObject value) {
            return Dictionary.TryGetValue (index, out value);
        }
    }
}
