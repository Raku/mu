using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class ConstructorObject : InstanceObject {
        private AnyObject Closure;

        public ConstructorObject (AnyObject closure) : base (RuntimeModule.Constructor) {
            Closure = closure;
        }

        public override AnyObject Invoke (params AnyObject [] arguments) {
            return New (this, arguments);
        }

        [TychoMethodSchema("constructor", "construct")]
        public static AnyObject ConstructSchema = new AnySchemaObject ();
        [TychoMethod ("constructor", "construct")]
        public static AnyObject Construct (AnyObject self, params AnyObject [] arguments) {
            ConstructorObject constructor = self.Expect<ConstructorObject> ();
            return constructor.Closure.Invoke (arguments);
        }

        [TychoMethodSchema ("constructor", "new")]
        public static AnyObject NewSchema = new AnySchemaObject ();
        [TychoMethod("constructor", "new")]
        public static AnyObject New (AnyObject self, params AnyObject [] arguments) {
            ConstructorObject constructor;
            if (self.TryCastTo (out constructor)) {
                AnyObject [] selfArguments = new AnyObject [arguments.Length + 1];
                AnyObject newSelf = RuntimeModule.CreatePrototype ();
                selfArguments [0] = newSelf;
                Array.Copy (arguments, 0, selfArguments, 1, arguments.Length);
                constructor.Closure.Invoke (selfArguments);
                return newSelf;
            } else {
                return new ConstructorObject (arguments [0]);
            }
        }
    }
}
