using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    class ProxyObject : PrototypeObject {
        AnyObject Aspect;

        public ProxyObject (AnyObject prototype, AnyObject aspect) : base (prototype) {
            Aspect = aspect;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            AnyObject [] aspectArgs = new AnyObject [arguments.Length + 2];
            aspectArgs [0] = self;
            aspectArgs [1] = name;
            arguments.CopyTo (aspectArgs, 2);
            return Aspect.Invoke (aspectArgs);
        }

        public override string ToString () {
            return Aspect.Invoke (Aspect, Symbols.RuntimeToString).ExpectValue<string> ();
        }
    }
}
