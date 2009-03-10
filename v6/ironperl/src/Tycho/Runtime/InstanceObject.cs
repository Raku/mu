using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class InstanceObject : AnyObject {
        public AnyObject Prototype { get; private set; }

        public InstanceObject (AnyObject prototype) {
            Prototype = prototype;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            return Prototype.InvokeMethod (this, name, arguments);
        }
    }
}
