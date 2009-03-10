using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;
using System.IO;

namespace Tycho.Runtime {
    public class RuntimeFunctions {
        [TychoModuleLoad]
        static void ModuleLoad (AnyObject module) {
            module [Symbols.RuntimeObject] = RuntimeModule.Object;
            module [Symbols.RuntimeClosure] = RuntimeModule.Closure;
            module [Symbols.RuntimeStructure] = RuntimeModule.Structure;
            module [Symbols.RuntimeList] = RuntimeModule.List;
            module [Symbols.RuntimeSet] = RuntimeModule.Set;
            module [Symbols.RuntimeDictionary] = RuntimeModule.Dictionary;
            module [Symbols.RuntimeTrue] = RuntimeModule.CreateBoolean (true);
            module [Symbols.RuntimeFalse] = RuntimeModule.CreateBoolean (false);
            module [Symbols.RuntimeNull] = RuntimeModule.Null;
            module [Symbols.RuntimeSymbol] = RuntimeModule.Symbol;
            module [Symbols.RuntimeModule] = RuntimeModule.Module;
            module [Symbols.RuntimeStackFrame] = RuntimeModule.StackFrame;
            module [Symbols.RuntimeModuleFrame] = RuntimeModule.ModuleFrame;
            module [Symbols.RuntimeDynamicStackFrame] = RuntimeModule.DynamicStackFrame;
            module [Symbols.RuntimeConstructor] = RuntimeModule.Constructor;

            module [Symbols.RuntimeIf] = new NativeFunction (IfSchema, If);
        }

        static AnyObject IfSchema = new ParametersSchemaObject (2, RuntimeModule.Boolean, RuntimeModule.Closure, RuntimeModule.Closure);
        static AnyObject If (params AnyObject [] arguments) {
            bool condition = arguments [0].ExpectValue<bool> ();

            if (condition) {
                AnyObject thenBlock = arguments [1];
                return thenBlock.Invoke ();
            } else if (arguments.Length >= 3) {
                AnyObject elseBlock = arguments [2];
                return elseBlock.Invoke ();
            } else {
                return RuntimeModule.Null;
            }
        }

        [TychoFunction2 ("while")]
        public static AnyObject While (AnyObject condBlock, AnyObject doBlock) {
            AnyObject lastValue = RuntimeModule.Null;

            while (condBlock.Invoke ().ExpectValue<bool> ()) {
                lastValue = doBlock.Invoke ();
            }

            return lastValue;
        }

        [TychoFunctionSchema ("switch")]
        public static AnyObject SwitchSchema = new AnySchemaObject ();
        [TychoFunction ("switch")]
        public static AnyObject Switch (params AnyObject [] arguments) {
            MultiMethod m = new MultiMethod ();

            for (int n = 1; n < arguments.Length; n++) {
                m.Methods.Add (arguments [n]);
            }

            AnyObject result;
            if (m.TryInvoke (out result, arguments [0])) {
                return result;
            } else {
                return RuntimeModule.Null;
            }
        }


        [TychoFunction2 ("print")]
        public static AnyObject Print (AnyObject argument) {
            TextWriter writer = ThreadContext.Current.GetProperty (Symbols.RuntimePrintStream).ExpectNative<TextWriter> ();
            writer.WriteLine (argument.ToString ());
            return RuntimeModule.Null;
        }

        [TychoFunction2 ("exit")]
        public static AnyObject Exit () {
            throw new ExitException ();
        }

        [TychoFunction2 ("sleep")]
        public static AnyObject Sleep (int milliseconds) {
            System.Threading.Thread.Sleep (milliseconds);

            return RuntimeModule.Null;
        }

        [TychoFunction2 ("match-assignment")]
        public static AnyObject MatchAssignment (AnyObject pattern, AnyObject results, AnyObject value) {
            if (pattern.Match (results, value)) {
                return value;
            } else {
                throw TychoException.AssignmentMatchFailed (pattern, value);
            }
        }

        [TychoFunctionSchema ("concatenate-to-string")]
        public static AnyObject ConcatenateToStringSchema = new AnySchemaObject ();
        [TychoFunction ("concatenate-to-string")]
        public static AnyObject ConcatenateToString (params AnyObject [] arguments) {
            var result = new StringBuilder ();

            foreach (AnyObject arg in arguments) {
                StringObject str;
                if (arg.TryCastTo (out str)) {
                    result.Append (str.Value);
                } else {
                    result.Append (arg);
                }
            }

            return result.ToString ();
        }

        [TychoFunction2("new-protocol")]
        public static AnyObject NewProtocol () {
            return RuntimeModule.CreateProtocol ();
        }

        [TychoFunction2 ("proxy")]
        public static AnyObject Proxy (AnyObject aspect) {
            return new ProxyObject (RuntimeModule.Object, aspect);
        }

        [TychoFunction2 ("using")]
        public static AnyObject Using (AnyObject disposableObject, AnyObject block) {
            using (disposableObject) {
                return block.Invoke ();
            }
        }

        [TychoFunction2 ("try")]
        public static AnyObject Try (AnyObject block, MultiMethod exceptionHandler, AnyObject finallyBlock) {
            try {
                return block.Invoke ();
            } catch (Exception e) {
                AnyObject result;
                if (exceptionHandler.TryInvoke (out result, RuntimeModule.ConvertToRuntime(e))) {
                    return result;
                }
                throw;
            } finally {
                finallyBlock.Invoke ();
            }
        }
    }
}
