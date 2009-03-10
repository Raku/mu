using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class NativeTypeSchemaObject<T> : SchemaObject {
        public override bool Match (AnyObject results, params AnyObject [] obj) {
            return obj [0] is NativeObject<T>;
        }
    }
}
