using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class UnlimitedParametersSchemaObject : SchemaObject {
        int _minimumArguments;

        public UnlimitedParametersSchemaObject (int minimumArguments) {
            _minimumArguments = minimumArguments;
        }

        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            return arguments.Length >= _minimumArguments;
        }
    }
}
