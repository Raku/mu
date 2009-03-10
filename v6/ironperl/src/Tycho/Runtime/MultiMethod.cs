using System.Collections.Generic;

namespace Tycho.Runtime {
    public class MultiMethod {
        public List<AnyObject> Methods { get; private set; }

        public MultiMethod () {
            Methods = new List<AnyObject> ();
        }

        public AnyObject Invoke (params AnyObject [] arguments) {
            AnyObject result;

            if (TryInvoke (out result, arguments)) {
                return result;
            } else {
                throw new NoMatchingMethodException (arguments);
            }
        }

        public bool TryInvoke (out AnyObject result, params AnyObject [] arguments) {
            StackFrameObject frame = new StackFrameObject (RuntimeModule.StackFrame, RuntimeModule.Null);

            foreach (AnyObject method in Methods) {
                if (method.ParametersSchema.Match (frame, arguments)) {
                    if (method is ClosureObject) {
                        ClosureObject closure = method as ClosureObject;
                        frame.OuterScope = closure.Context;
                        result = closure.InvokeWithFrame (frame);
                        return true;
                    } else if (method is NativeOperation) {
                        result = (method as NativeOperation).InvokeWithoutSchema (arguments);
                        return true;
                    } else {
                        result = method.Invoke (arguments);
                        return true;
                    }
                }
            }

            result = null;
            return false;
        }

        public class NoMatchingMethodException : TychoException {
            public AnyObject [] Arguments { get; private set; }

            public NoMatchingMethodException (params AnyObject [] arguments) : base ("no matching arguments") {
                Arguments = arguments;
            }
        }
    }
}
