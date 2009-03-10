using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class LazyObject : AnyObject {
        private AnyObject InternalOperation;
        private AnyObject CachedValue;

        public LazyObject (AnyObject operation) {
            Operation = operation;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeGetProperty && arguments[0] == Symbols.RuntimeOperation) {
                return Operation;
            } else if (name == Symbols.RuntimeGetProperty && arguments[0] == Symbols.RuntimeProperties) {
                AnyObject props = RuntimeModule.CreateSet ((IEnumerable<AnyObject>) Value.Properties);
                props.Add (Symbols.RuntimeOperation);
                return props;
            } else if (name == Symbols.RuntimeSetProperty && arguments[0] == Symbols.RuntimeOperation) {
                return Operation = arguments[1];
            } else if (name == Symbols.RuntimeClearCache) {
                ClearCache ();
                return RuntimeModule.Null;
            } else {
                return Value.InvokeMethod (Value, name, arguments);
            }
        }

        private AnyObject Value {
            get {
                if (CachedValue != null) {
                    return CachedValue;
                } else {
                    return CachedValue = Operation.Invoke ();
                }
            }
        }

        public AnyObject Operation {
            get {
                return InternalOperation;
            }
            set {
                InternalOperation = value;
                CachedValue = null;
            }
        }

        public override string  ToString(HashSet<AnyObject> done) {
            return Value.ToString (done);
        }

        public override bool TryCastTo<T> (out T result) {
            return Value.TryCastTo<T> (out result);
        }

        public void ClearCache () {
            CachedValue = null;
        }
    }
}
