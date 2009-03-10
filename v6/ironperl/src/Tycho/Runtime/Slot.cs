using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    class Slot {
    }

    class MethodSlot : Slot {
        MultiMethod MultiMethod;

        public MethodSlot () {
            MultiMethod = new MultiMethod ();
        }

        public List<AnyObject> Methods {
            get {
                return MultiMethod.Methods;
            }
        }

        public bool TryInvoke (out AnyObject result, params AnyObject [] arguments) {
            return MultiMethod.TryInvoke (out result, arguments);
        }
    }

    class FieldSlot : Slot {
        public AnyObject Value { get; set; }

        public FieldSlot (AnyObject value) {
            Value = value;
        }
    }

    class PropertySlot : Slot {
        public AnyObject Getter { get; set; }
        public AnyObject Setter { get; set; }

        public AnyObject GetValue (AnyObject self) {
            return Getter.Invoke (self);
        }

        public void SetValue (AnyObject self, AnyObject value) {
            Setter.Invoke (self, value);
        }
    }
}
