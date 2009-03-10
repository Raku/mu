using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class NativeMethodSchema {
        public AnyObject ParametersSchema { get; set; }
        public AnyObject ResultSchema { get; set; }

        public NativeMethodSchema (AnyObject parametersSchema, AnyObject resultSchema) {
            ParametersSchema = parametersSchema;
            ResultSchema = resultSchema;
        }
    }
}
