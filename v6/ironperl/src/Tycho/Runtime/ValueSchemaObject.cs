using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class ValueSchemaObject<T> : SchemaObject {
        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            ValueObject<T> ignored;
            return arguments [0].TryCastTo<ValueObject<T>> (out ignored);
        }
    }
}
