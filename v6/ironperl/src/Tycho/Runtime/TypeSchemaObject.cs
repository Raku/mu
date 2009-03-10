using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class TypeSchemaObject<T> : SchemaObject where T : AnyObject {
        public override bool Match (AnyObject results, params AnyObject [] args) {
            T result;
            return args [0].TryCastTo<T> (out result);
        }
    }
}
