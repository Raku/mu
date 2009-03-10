using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public abstract class Operation : AnyObject {
        private AnyObject InternalParametersSchema;

        public override AnyObject ParametersSchema {
            get { return InternalParametersSchema; }
        }

        public Operation (AnyObject schema) {
            InternalParametersSchema = schema;
        }
    }

    public abstract class NativeOperation : Operation {
        public NativeOperation (AnyObject schema) : base (schema) { }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeInvoke) {
                return Invoke (arguments);
            } else {
                return RuntimeModule.Operation.InvokeMethod (self, name, arguments);
            }
        }

        public override AnyObject Invoke (params AnyObject [] arguments) {
            if (ParametersSchema.Match (RuntimeModule.Null, arguments)) {
                return InvokeWithoutSchema (arguments);
            } else {
                throw TychoException.InvalidArguments ();
            }
        }

        public abstract AnyObject InvokeWithoutSchema (AnyObject [] arguments);

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                return RuntimeModule.CreateSet ();
            }
        }
    }

    public delegate AnyObject NativeMethodDelegate (AnyObject self, params AnyObject [] arguments);

    public class NativeMethod : NativeOperation {
        NativeMethodDelegate Method;

        public NativeMethod (AnyObject schema, NativeMethodDelegate method)
            : base (schema) {
            Method = method;
        }

        public override AnyObject InvokeWithoutSchema (params AnyObject [] arguments) {
            AnyObject [] invokeArguments = new AnyObject [arguments.Length - 1];
            Array.ConstrainedCopy (arguments, 1, invokeArguments, 0, arguments.Length - 1);
            return Method (arguments [0], invokeArguments);
        }
    }

    public delegate AnyObject NativeFunctionDelegate (params AnyObject [] arguments);

    public class NativeFunction : NativeOperation {
        NativeFunctionDelegate _function;

        public NativeFunction (AnyObject schema, NativeFunctionDelegate function)
            : base (schema) {
            _function = function;
        }

        public override AnyObject InvokeWithoutSchema (params AnyObject [] arguments) {
            return _function (arguments);
        }
    }
}
