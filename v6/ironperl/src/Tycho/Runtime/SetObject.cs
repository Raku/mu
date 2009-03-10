using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class SetObject : InstanceObject, IEnumerable<AnyObject> {
        ISet<AnyObject> Contents = new Set<AnyObject> ();

        public SetObject (AnyObject prototype) : base (prototype) { }

        public SetObject (AnyObject prototype, IEnumerable<AnyObject> items)
            : this (prototype) {
            Contents.UnionWith (items);
        }

        public override void Add (AnyObject a) {
            Contents.Add (a);
        }

        public override bool Remove (AnyObject a) {
            return Contents.Remove (a);
        }

        public override int Count {
            get {
                return Contents.Count;
            }
        }

        public override bool Contains (AnyObject a) {
            return Contents.Contains (a);
        }

        public override string ToString (HashSet<AnyObject> done) {
            if (ToStringNoRecurse (done)) {
                string contents = String.Join (", ", Contents.Select (item => item.ToString (done)).OrderBy (str => str).ToArray ());

                return "set {" + contents + "}";
            } else {
                return "...";
            }
        }

        public override AnyObject ShallowCopy () {
            return new SetObject (Prototype, Contents);
        }

        [TychoMethodSchema ("set", "add")]
        static AnyObject PrototypeAddSchema = new ParametersSchemaObject (new AnySchemaObject (), new AnySchemaObject ());
        [TychoMethod ("set", "add")]
        static AnyObject PrototypeAdd (AnyObject self, params AnyObject [] args) {
            self.Add (args [0]);
            return args [0];
        }

        [TychoMethodSchema ("set", "remove")]
        static AnyObject PrototypeRemoveSchema = new ParametersSchemaObject (new AnySchemaObject (), new AnySchemaObject ());
        [TychoMethod ("set", "remove")]
        static AnyObject PrototypeRemove (AnyObject self, params AnyObject [] args) {
            return RuntimeModule.CreateBoolean (self.Remove (args [0]));
        }

        [TychoMethodSchema ("set", "count")]
        static AnyObject GetCountSchema = new AnySchemaObject ();
        [TychoGetter ("set", "count")]
        static AnyObject GetCount (AnyObject self, params AnyObject [] args) {
            return RuntimeModule.CreateInteger (self.Count);
        }

        [TychoMethodSchema ("set", "contains")]
        static AnyObject PrototypeContainsSchema = new ParametersSchemaObject (new AnySchemaObject (), new AnySchemaObject ());
        [TychoMethod ("set", "contains")]
        static AnyObject PrototypeContains (AnyObject self, params AnyObject [] args) {
            return RuntimeModule.CreateBoolean (self.Contains (args [0]));
        }

        [TychoMethodSchema ("set", "new")]
        static AnyObject NewSchema = new UnlimitedParametersSchemaObject (1);
        [TychoMethod ("set", "new")]
        static AnyObject New (AnyObject self, params AnyObject [] args) {
            return RuntimeModule.CreateSet (args);
        }

        [TychoMethodSchema ("set", "object-references")]
        static AnyObject GetObjectReferencesSchema = new AnySchemaObject ();
        [TychoGetter ("set", "object-references")]
        static AnyObject GetObjectReferences (AnyObject self, params AnyObject [] arguments) {
            return self;
        }

        [TychoGetter2 ("set", "enumerator")]
        public static IEnumerator<AnyObject> GetEnumerator (SetObject self) {
            return ((IEnumerable<AnyObject>) self).GetEnumerator ();
        }

        [TychoMethod2 ("set", "intersection")]
        public static AnyObject Intersection (SetObject self, SetObject other) {
            ISet<AnyObject> intersection = new Set<AnyObject> (self.Contents);
            intersection.IntersectWith (other.Contents);
            return RuntimeModule.CreateSet (intersection);
        }

        [TychoMethod2 ("set", "union")]
        public static AnyObject Union (SetObject self, SetObject other) {
            ISet<AnyObject> union = new Set<AnyObject> (self.Contents);
            union.UnionWith (other.Contents);
            return RuntimeModule.CreateSet (union);
        }

        [TychoMethod2 ("set", "minus")]
        public static AnyObject Minus (SetObject self, SetObject other) {
            ISet<AnyObject> remainder = new Set<AnyObject> (self.Contents);
            remainder.ExceptWith (other.Contents);
            return RuntimeModule.CreateSet (remainder);
        }

        [TychoMethod2 ("set", "multiply")]
        public static AnyObject Multiply (SetObject self, SetObject other) {
            ISet<AnyObject> product = new Set<AnyObject> ();

            foreach (var a in self.Contents) {
                foreach (var b in other.Contents) {
                    product.Add (RuntimeModule.CreateList (a, b));
                }
            }
            return RuntimeModule.CreateSet (product);
        }

        [TychoMethod2 ("set", "less-than")]
        public static bool LessThan (SetObject self, SetObject other) {
            return self.Contents.IsProperSubsetOf (other);
        }

        [TychoMethod2 ("set", "greater-than")]
        public static bool GreaterThan (SetObject self, SetObject other) {
            return self.Contents.IsProperSupersetOf (other);
        }

        [TychoMethod2 ("set", "less-than-equal-to")]
        public static bool LessThanEqualTo (SetObject self, SetObject other) {
            return self.Contents.IsSubsetOf (other);
        }

        [TychoMethod2 ("set", "greater-than-equal-to")]
        public static bool GreaterThanEqualTo (SetObject self, SetObject other) {
            return self.Contents.IsSupersetOf (other);
        }

        #region IEnumerable Members

        IEnumerator<AnyObject> IEnumerable<AnyObject>.GetEnumerator () {
            return Contents.GetEnumerator ();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator () {
            return Contents.GetEnumerator ();
        }

        #endregion
    }
}
