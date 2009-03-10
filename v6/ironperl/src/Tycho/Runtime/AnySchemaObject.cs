using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class AnySchemaObject : SchemaObject {
        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            return true;
        }
    }
}
