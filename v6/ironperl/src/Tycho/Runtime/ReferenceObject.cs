using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;

namespace Tycho.Runtime {
    public class ReferenceObject : SimpleObject {
        public AnyObject Target { get; private set; }

        public ReferenceObject (AnyObject target) {
            Target = target;
        }

        public override string ToString (HashSet<AnyObject> done) {
            return "ref";
        }
    }
}
