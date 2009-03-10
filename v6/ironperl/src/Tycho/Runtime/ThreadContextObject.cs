using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    class ThreadContextObject : AnyObject {
        Dictionary<AnyObject, AnyObject> Contents = new Dictionary<AnyObject, AnyObject> ();
        public override AnyObject ProxyTarget { get; set; }

        public ThreadContextObject (AnyObject parent) {
            ProxyTarget = parent;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeGetProperty) {
                return GetProperty (self, arguments [0]);
            } else if (name == Symbols.RuntimeSetProperty) {
                SetProperty (self, arguments [0], arguments [1]);
                return arguments [1];
            } else {
                throw TychoException.NoSuchMethod (name);
            }
        }

        public override AnyObject GetProperty (AnyObject self, AnyObject name) {
            AnyObject value;

            if (Contents.TryGetValue (name, out value)) {
                return value;
            } else if (ProxyTarget != null) {
                return ProxyTarget.GetProperty (self, name);
            } else {
                throw TychoException.NoSuchProperty (name);
            }
        }

        public override void SetProperty (AnyObject self, AnyObject name, AnyObject value) {
            Contents [name] = value;
        }
    }
}
