using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public abstract class SchemaObject : AnyObject {
        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeMatch) {
                AnyObject [] matchArgs = new AnyObject [arguments.Length - 1];
                Array.ConstrainedCopy (arguments, 1, matchArgs, 0, arguments.Length - 1);
                return RuntimeModule.CreateBoolean (Match (arguments [0], matchArgs));
            } else {
                throw TychoException.NoSuchMethod (name);
            }
        }
    }
}
