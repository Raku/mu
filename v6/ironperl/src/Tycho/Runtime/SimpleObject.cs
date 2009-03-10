using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class SimpleObject : AnyObject {
        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            throw TychoException.NoSuchMethod (name);
        }
    }
}
