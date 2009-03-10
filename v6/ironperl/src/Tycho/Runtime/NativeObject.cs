using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class NativeObject<T> : SimpleObject {
        public T NativeInstance { get; private set; }

        public NativeObject (T obj) {
            NativeInstance = obj;
        }

        public override string ToString (HashSet<AnyObject> done) {
            return NativeInstance.ToString ();
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                return new AnyObject [0];
            }
        }
    }
}
