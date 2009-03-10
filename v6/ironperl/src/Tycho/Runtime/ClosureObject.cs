using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Parser;
using Tycho.Utilities;
using Tycho.Parser.Tokens;

namespace Tycho.Runtime {
    public enum ByteCode {
        Load,
        LoadRemote,
        LoadInteger,
        LoadConstant,
        LoadStackFrame,
        LoadLast,
        Store,
        StoreRemote,
        Pop,
        InvokeMethod,
        InvokeMethodWithList,
        Invoke,
        InvokeWithList,
        InvokePlus,
        InvokeMinus,
        InvokeMultiply,
        InvokeDivide,
        InvokeLessThan,
        InvokeLessThanEqualTo,
        InvokeGreaterThan,
        InvokeGreaterThanEqualTo,
        Return
    }

    public class ClosureObject : Operation {
        public AnyObject Context { get; private set; }
        ByteCode [] Code;
        AnyObject [] Constants;
        public bool IsTopLevel { get; set; }
        private SourceLocation InternalSourceLocation;

        public override SourceLocation SourceLocation {
            get { return InternalSourceLocation; }
            set { throw new TychoException ("cannot modify source location of closure"); }
        }

        public ClosureObject (ByteCode[] code, AnyObject[] constants, AnyObject parametersSchema, AnyObject context, SourceLocation sourceLocation)
            : base (parametersSchema) {
            Code = code;
            Context = context;
            Constants = constants;
            IsTopLevel = false;
            InternalSourceLocation = sourceLocation;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeInvoke) {
                return Invoke (arguments);
            } else {
                return RuntimeModule.Closure.InvokeMethod (self, name, arguments);
            }
        }

        public override AnyObject Invoke (params AnyObject [] arguments) {
            AnyObject frame;

            if (IsTopLevel) {
                frame = Context;
            } else {
                frame = new StackFrameObject (RuntimeModule.StackFrame, Context);
            }

            if (!ParametersSchema.Match (frame, arguments)) {
                throw TychoException.InvalidArguments ();
            }
            Console.WriteLine(this.PrettyPrint());
            return InvokeWithFrame (frame);
        }

        public AnyObject InvokeWithFrame (AnyObject frame) {
            Stack<AnyObject> stack = new Stack<AnyObject> (10);
            int codeIndex = 0;
            while (codeIndex < Code.Length) {
                switch (Code [codeIndex]) {
                    case ByteCode.Load: {
                            AnyObject name = stack.Pop ();
                            stack.Push (frame.GetVariable (frame, name, 0));
                        }
                        break;
                    case ByteCode.LoadRemote: {
                            AnyObject index = stack.Pop ();
                            AnyObject name = stack.Pop ();
                            stack.Push (frame.GetVariable (frame, name, index.ExpectValue<int> ()));
                        }
                        break;
                    case ByteCode.LoadInteger:
                        codeIndex++;
                        stack.Push (RuntimeModule.CreateInteger ((int) Code [codeIndex]));
                        break;
                    case ByteCode.LoadConstant: {
                            codeIndex++;
                            int constantIndex = (int) Code [codeIndex];
                            stack.Push (Constants [constantIndex]);
                        }
                        break;
                    case ByteCode.LoadStackFrame:
                        stack.Push (frame);
                        break;
                    case ByteCode.LoadLast:
                        stack.Push (stack.Peek ());
                        break;
                    case ByteCode.Store: {
                            AnyObject name = stack.Pop ();
                            AnyObject value = stack.Pop ();
                            frame.SetVariable (frame, name, 0, value);
                        }
                        break;
                    case ByteCode.StoreRemote: {
                            AnyObject index = stack.Pop ();
                            AnyObject name = stack.Pop ();
                            AnyObject value = stack.Pop ();

                            frame.SetVariable (frame, name, index.ExpectValue<int> (), value);
                        }
                        break;
                    case ByteCode.Pop:
                        stack.Pop ();
                        break;
                    case ByteCode.InvokeMethod: {
                            HandleInvokeMethod (stack, false);
                        }
                        break;
                    case ByteCode.InvokeMethodWithList: {
                            HandleInvokeMethod (stack, true);
                        }
                        break;
                    case ByteCode.Invoke: {
                            HandleInvoke (stack, false);
                        }
                        break;
                    case ByteCode.InvokeWithList: {
                            HandleInvoke (stack, true);
                        }
                        break;
                    case ByteCode.InvokePlus: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.Plus (arg));
                        }
                        break;
                    case ByteCode.InvokeMinus: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.Minus (arg));
                        }
                        break;
                    case ByteCode.InvokeMultiply: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.Multiply (arg));
                        }
                        break;
                    case ByteCode.InvokeDivide: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.Divide (arg));
                        }
                        break;
                    case ByteCode.InvokeLessThan: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.LessThan (arg));
                        }
                        break;
                    case ByteCode.InvokeLessThanEqualTo: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.LessThanEqualTo (arg));
                        }
                        break;
                    case ByteCode.InvokeGreaterThan: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.GreaterThan (arg));
                        }
                        break;
                    case ByteCode.InvokeGreaterThanEqualTo: {
                            AnyObject arg = stack.Pop ();
                            AnyObject self = stack.Pop ();

                            stack.Push (self.GreaterThanEqualTo (arg));
                        }
                        break;
                    case ByteCode.Return:
                        return stack.Pop ();
                }

                codeIndex++;
            }

            return stack.Pop ();
        }

        private void HandleInvoke (Stack<AnyObject> stack, bool withListArguments) {
            AnyObject [] methodArguments = GetMethodArguments (stack, withListArguments);

            AnyObject target = stack.Pop ();

            stack.Push (target.Invoke (methodArguments));
        }

        private void HandleInvokeMethod (Stack<AnyObject> stack, bool withListArguments) {
            AnyObject [] methodArguments = GetMethodArguments (stack, withListArguments);

            AnyObject name = stack.Pop ();
            AnyObject self = stack.Pop ();
            AnyObject target = stack.Pop ();

            stack.Push (target.InvokeMethod (self, name, methodArguments));
        }

        private AnyObject[] GetMethodArguments (Stack<AnyObject> stack, bool withListArguments) {
            int nArgs = stack.Pop ().ExpectValue<int> ();

            if (withListArguments) {
                List<AnyObject> methodArguments = new List<AnyObject> ();
                for (int n = 0; n < nArgs - 1; n++) {
                    methodArguments.Add (stack.Pop ());
                }
                methodArguments.AddRange (stack.Pop ());
                return methodArguments.ToArray ();
            } else {
                AnyObject[] methodArguments = new AnyObject[nArgs];
                for (int n = 0; n < nArgs; n++) {
                    methodArguments[n] = stack.Pop ();
                }
                return methodArguments;
            }
        }

        public string PrettyPrint () {
            System.IO.StringWriter writer = new System.IO.StringWriter ();

            int codeIndex = 0;
            while (codeIndex < Code.Length) {
                writer.Write (codeIndex + ": ");

                ByteCode byteCode = Code [codeIndex];
                switch (byteCode) {
                    case ByteCode.Load:
                    case ByteCode.LoadRemote:
                    case ByteCode.LoadStackFrame:
                    case ByteCode.LoadLast:
                    case ByteCode.Store:
                    case ByteCode.StoreRemote:
                    case ByteCode.Pop:
                    case ByteCode.InvokeMethod:
                    case ByteCode.InvokeMethodWithList:
                    case ByteCode.Return:
                        writer.WriteLine (byteCode);
                        break;
                    case ByteCode.LoadConstant:
                    case ByteCode.LoadInteger:
                        writer.WriteLine (byteCode + " " + (int) Code [++codeIndex]);
                        break;
                }

                codeIndex++;
            }

            writer.WriteLine ();
            writer.WriteLine ("consts:");

            int constIndex = 0;
            foreach (AnyObject c in Constants) {
                writer.WriteLine (constIndex++ + ": " + c);
            }

            return writer.ToString ();
        }

        [TychoMethodSchema ("closure", "new")]
        static AnyObject NewClosureSchema = new ParametersSchemaObject (new AnySchemaObject (), new AnySchemaObject (), new AnySchemaObject (), new AnySchemaObject (), new AnySchemaObject ());
        [TychoMethod ("closure", "new")]
        static AnyObject NewClosure (AnyObject self, params AnyObject [] arguments) {
            AnyObject codeBody = arguments [0];
            AnyObject codesObject = codeBody.GetProperty (Symbols.RuntimeCode);
            AnyObject constantsObject = codeBody.GetProperty (Symbols.RuntimeConstants);
            AnyObject schema = arguments [1];
            AnyObject context = arguments [2];
            AnyObject sloc = arguments [3];

            ByteCode [] codes = new ByteCode [codesObject.Count];
            int n = 0;
            foreach (AnyObject code in codesObject) {
                codes [n++] = (ByteCode) code.ExpectValue<int> ();
            }

            AnyObject [] constants = new AnyObject [constantsObject.Count];
            n = 0;
            foreach (AnyObject constant in constantsObject) {
                constants [n++] = constant;
            }

            return new ClosureObject (codes, constants, schema, context, sloc.ToSourceLocation ());
        }

        [TychoMethodSchema ("closure", "new-parameters-schema")]
        static AnyObject NewParametersSchemaSchema = new UnlimitedParametersSchemaObject (0);
        [TychoMethod ("closure", "new-parameters-schema")]
        static AnyObject NewParametersSchema (AnyObject self, params AnyObject [] arguments) {
            bool listParameters = arguments [0].ExpectValue<bool> ();
            AnyObject [] parameters = new AnyObject [arguments.Length - 1];
            Array.ConstrainedCopy (arguments, 1, parameters, 0, parameters.Length);
            return new ParametersSchemaObject (listParameters, parameters);
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> set = new HashSet<AnyObject> ();
                set.Add (Context);
                set.Add (ParametersSchema);
                return set;
            }
        }

        [TychoMethod2 ("closure", "match")]
        public static bool PrototypeMatch (AnyObject self, AnyObject result, AnyObject arg) {
            return self == RuntimeModule.Closure && arg is ClosureObject;
        }

        [TychoMethodSchema ("closure", "invoke")]
        static AnyObject PrototypeInvokeSchema = new AnySchemaObject ();
        [TychoMethod ("closure", "invoke")]
        public static AnyObject PrototypeInvoke (AnyObject self, params AnyObject [] arguments) {
            return self.Invoke (arguments);
        }
    }
}
