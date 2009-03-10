using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class ProtocolObject : SchemaObject {
        public override bool Match (AnyObject results, params AnyObject [] objs) {
            AnyObject obj = objs[0];

            if (obj.HasProperty (Symbols.RuntimeProtocolsImplemented)) {
                return obj.GetProperty (Symbols.RuntimeProtocolsImplemented).Contains (this);
            } else {
                return false;
            }
        }
    }
}
