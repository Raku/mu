using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class NullObject : SimpleObject {
        public override bool IsNull { get { return true; } }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            throw new TychoNullPointerException ();
        }

        public override string ToString (HashSet<AnyObject> done) {
            return "null";
        }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }
    }
}
