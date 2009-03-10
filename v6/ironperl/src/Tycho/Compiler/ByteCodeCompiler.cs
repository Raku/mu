using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Parser;
using Tycho.Parser.Tokens;
using Tycho.Runtime;
using IronPerl;

namespace Tycho.Compiler {
    class ByteCodeCompiler {
        static Symbol CompilerMethodCall = Namespaces.Parser.Get ("method-call"),
            CompilerExpression = Namespaces.Parser.Get ("expression"),
            CompilerTarget = Namespaces.Parser.Get ("target"),
            CompilerListArguments = Namespaces.Parser.Get ("list-arguments"),
            CompilerSelf = Namespaces.Parser.Get ("self"),
            CompilerName = Namespaces.Parser.Get ("name"),
            CompilerArguments = Namespaces.Parser.Get ("arguments"),
            CompilerStore = Namespaces.Parser.Get ("store"),
            CompilerLoad = Namespaces.Parser.Get ("load"),
            CompilerLoadConstant = Namespaces.Parser.Get ("load-constant"),
            CompilerFrameIndex = Namespaces.Parser.Get ("frame-index"),
            CompilerConstant = Namespaces.Parser.Get ("constant"),
            CompilerLoadStackFrame = Namespaces.Parser.Get ("load-stack-frame"),
            CompilerLoadInteger = Namespaces.Parser.Get ("load-integer"),
            CompilerInteger = Namespaces.Parser.Get ("integer"),
            CompilerLoadCodeBody = Namespaces.Parser.Get ("load-code-body"),
            CompilerParametersSchema = Namespaces.Parser.Get ("parameters-schema"),
            CompilerBody = Namespaces.Parser.Get ("body"),
            CompilerPop = Namespaces.Parser.Get ("pop"),
            CompilerNop = Namespaces.Parser.Get ("nop"),
            CompilerReturn = Namespaces.Parser.Get ("return");

        TermTransform<ByteCodeOutput> Transform;
        private Dictionary<AnyObject, ByteCode> FastOpCodes;

        public ByteCodeCompiler () {
            Transform = new TermTransform<ByteCodeOutput> ();
            Transform.PreTransform = CheckForSourceLocation;

            SetUpFastOpCodes ();

            Transform.AddTransform (CompilerMethodCall, (output, term) => {
                CompileInvokeMethod (output, term);

                return null;
            });
            Transform.AddTransform (CompilerStore, (output, term) => {
                Compile (output, term [CompilerExpression]);

                // leave the result of the expression on the stack after the operation is complete
                output.ByteCodes.Add (ByteCode.LoadLast);

                Compile (output, term [CompilerName]);

                if (term.Has (CompilerFrameIndex)) {
                    Compile (output, term [CompilerFrameIndex]);

                    output.ByteCodes.Add (ByteCode.StoreRemote);
                } else {
                    output.ByteCodes.Add (ByteCode.Store);
                }

                return null;
            });
            Transform.AddTransform (CompilerLoadConstant, (output, term) => {
                EmitLoadConstant (output, term [CompilerConstant]);

                return null;
            });
            Transform.AddTransform (CompilerLoadInteger, (output, term) => {
                output.ByteCodes.Add (ByteCode.LoadInteger);
                output.ByteCodes.Add ((ByteCode) term [CompilerInteger].ExpectValue<int> ());

                return null;
            });
            Transform.AddTransform (CompilerLoad, (output, term) => {
                Compile (output, term [CompilerName]);

                if (term.Has (CompilerFrameIndex)) {
                    Compile (output, term [CompilerFrameIndex]);

                    output.ByteCodes.Add (ByteCode.LoadRemote);
                } else {
                    output.ByteCodes.Add (ByteCode.Load);
                }

                return null;
            });
            Transform.AddTransform (CompilerLoadCodeBody, (output, term) => {
                ByteCodeOutput closureOutput = new ByteCodeOutput ();

                Transform.Transform (closureOutput, term [CompilerBody]);

                AnyObject codeBody = RuntimeModule.CreateStructure ();
                AnyObject codes = RuntimeModule.CreateList ();
                foreach (ByteCode code in closureOutput.GetCode ()) {
                    codes.Add (RuntimeModule.CreateInteger ((int) code));
                }

                AnyObject constants = RuntimeModule.CreateList ();
                foreach (AnyObject constant in closureOutput.GetConstants ()) {
                    constants.Add (constant);
                }

                codeBody.SetProperty (Symbols.RuntimeCode, codes);
                codeBody.SetProperty (Symbols.RuntimeConstants, constants);

                EmitLoadConstant (output, codeBody);

                return null;
            });
            Transform.AddTransform (CompilerLoadStackFrame, (output, term) => {
                output.ByteCodes.Add (ByteCode.LoadStackFrame);
                return null;
            });
            Transform.AddTransform (CompilerPop, (output, term) => {
                output.ByteCodes.Add (ByteCode.Pop);
                return null;
            });
            Transform.AddTransform (CompilerNop, (output, term) => {
                return null;
            });
            Transform.AddTransform (CompilerReturn, (output, term) => {
                Transform.Transform (output, term [CompilerExpression]);
                output.ByteCodes.Add (ByteCode.Return);
                return null;
            });
        }

        private void SetUpFastOpCodes () {
            FastOpCodes = new Dictionary<AnyObject, ByteCode>();
            FastOpCodes [Symbols.RuntimePlus] = ByteCode.InvokePlus;
            FastOpCodes [Symbols.RuntimeMinus] = ByteCode.InvokeMinus;
            FastOpCodes [Symbols.RuntimeMultiply] = ByteCode.InvokeMultiply;
            FastOpCodes [Symbols.RuntimeDivide] = ByteCode.InvokeDivide;
            FastOpCodes [Symbols.RuntimeLessThan] = ByteCode.InvokeLessThan;
            FastOpCodes [Symbols.RuntimeLessThanEqualTo] = ByteCode.InvokeLessThanEqualTo;
            FastOpCodes [Symbols.RuntimeGreaterThan] = ByteCode.InvokeGreaterThan;
            FastOpCodes [Symbols.RuntimeGreaterThanEqualTo] = ByteCode.InvokeGreaterThanEqualTo;
        }

        private void CompileInvokeMethod (ByteCodeOutput output, TermWrapper term) {
            bool specifiesSelf = term.Has (CompilerSelf);

            if (!specifiesSelf && TryCompileInvokeFastMethod (output, term)) {
                return;
            }

            Transform.Transform (output, term [CompilerTarget]);

            if (!specifiesSelf) {
                output.ByteCodes.Add (ByteCode.LoadLast);
            } else {
                Transform.Transform (output, term [CompilerSelf]);
            }

            Transform.Transform (output, term [CompilerName]);

            AnyObject arguments = term [CompilerArguments];
            for (int n = arguments.Count - 1; n >= 0; n--) {
                Transform.Transform (output, arguments [n]);
            }

            output.ByteCodes.Add (ByteCode.LoadInteger);
            output.ByteCodes.Add ((ByteCode) term [CompilerArguments].Count);

            ByteCode invocation;
            if ((bool) term.Term.GetProperty (CompilerListArguments)) {
                invocation = ByteCode.InvokeMethodWithList;
            } else {
                invocation = ByteCode.InvokeMethod;
            }
            output.ByteCodes.Add (invocation);
        }

        private bool TryCompileInvokeFastMethod (ByteCodeOutput output, TermWrapper term) {
            AnyObject methodName;
            if (TryGetConstant (term, out methodName)) {
                ByteCode fastOpCode;

                if (methodName == Symbols.RuntimeInvoke) {
                    Transform.Transform (output, term [CompilerTarget]);

                    AnyObject arguments = term [CompilerArguments];
                    for (int n = arguments.Count - 1; n >= 0; n--) {
                        Transform.Transform (output, arguments [n]);
                    }

                    output.ByteCodes.Add (ByteCode.LoadInteger);
                    output.ByteCodes.Add ((ByteCode) term [CompilerArguments].Count);

                    ByteCode invocation;
                    if ((bool) term.Term.GetProperty (CompilerListArguments)) {
                        invocation = ByteCode.InvokeWithList;
                    } else {
                        invocation = ByteCode.Invoke;
                    }
                    output.ByteCodes.Add (invocation);

                    return true;
                } else if (FastOpCodes.TryGetValue (methodName, out fastOpCode) && term [CompilerArguments].Count == 1) {
                    Transform.Transform (output, term [CompilerTarget]);
                    Transform.Transform (output, term [CompilerArguments] [0]);
                    output.ByteCodes.Add (fastOpCode);

                    return true;
                }
            }

            return false;
        }

        private bool TryGetConstant (TermWrapper term, out AnyObject constant) {
            AnyObject nameTerm = term[CompilerName];

            if (nameTerm.GetProperty (Symbols.ParserTermName) == CompilerLoadConstant) {
                constant = nameTerm.GetProperty (CompilerConstant);
                return true;
            } else {
                constant = null;
                return false;
            }
        }

        private static void EmitLoadConstant (ByteCodeOutput output, AnyObject constant) {
            output.ByteCodes.Add (ByteCode.LoadConstant);
            output.ByteCodes.Add ((ByteCode) output.AddConstant (constant));
        }

        public ByteCodeOutput GenerateCode (AnyObject term) {
            ByteCodeOutput output = new ByteCodeOutput ();
            Compile (output, term);
            return output;
        }

        public ClosureObject GenerateClosure (AnyObject term, AnyObject contextStackFrame, SourceLocation sloc) {
            ByteCodeOutput output = GenerateCode (term);
            return new ClosureObject (output.GetCode (), output.GetConstants (), new ParametersSchemaObject (), contextStackFrame, sloc);
        }

        AnyObject Compile (ByteCodeOutput output, AnyObject term) {
            return Transform.Transform (output, term);
        }

        public static AnyObject MethodCall (AnyObject term, AnyObject target, AnyObject name, params AnyObject [] arguments) {
            return MethodCall (term, false, target, name, arguments);
        }

        public static AnyObject MethodCall (AnyObject term, bool listArgs, AnyObject target, AnyObject name, params AnyObject [] arguments) {
            AnyObject call = CompilerModule.CreateTerm (term.SourceLocation);
            call.SetProperty (Symbols.ParserTermName, CompilerMethodCall);

            call.SetProperty (CompilerTarget, target);
            call.SetProperty (CompilerListArguments, RuntimeModule.CreateBoolean (listArgs));
            call.SetProperty (CompilerName, name);
            call.SetProperty (CompilerArguments, CompilerModule.CreateTermList (arguments));
            return call;
        }

        public static AnyObject MethodCallExplicitSelf (AnyObject term, AnyObject target, AnyObject self, AnyObject name, params AnyObject [] arguments) {
            AnyObject call = MethodCall (term, target, name, arguments);
            call.SetProperty (CompilerSelf, self);
            return call;
        }

        public static AnyObject Store (AnyObject term, AnyObject expression, AnyObject name, int frameIndex) {
            AnyObject store = CompilerModule.CreateTerm (term.SourceLocation);
            store.SetProperty (Symbols.ParserTermName, CompilerStore);

            store.SetProperty (CompilerName, name);
            store.SetProperty (CompilerFrameIndex, LoadInteger (term, frameIndex));
            store.SetProperty (CompilerExpression, expression);
            return store;
        }

        public static AnyObject Load (AnyObject term, AnyObject name, int frameIndex) {
            AnyObject load = CompilerModule.CreateTerm (term.SourceLocation);
            load.SetProperty (Symbols.ParserTermName, CompilerLoad);

            load.SetProperty (CompilerName, name);
            load.SetProperty (CompilerFrameIndex, LoadInteger (term, frameIndex));
            return load;
        }

        public static AnyObject LoadConstant (AnyObject term, AnyObject constant) {
            AnyObject load = CompilerModule.CreateTerm (term.SourceLocation);
            load.SetProperty (Symbols.ParserTermName, CompilerLoadConstant);

            load.SetProperty (CompilerConstant, constant);
            return load;
        }

        public static AnyObject LoadStackFrame (AnyObject term) {
            AnyObject load = CompilerModule.CreateTerm (term.SourceLocation);

            load.SetProperty (Symbols.ParserTermName, CompilerLoadStackFrame);
            return load;
        }

        public static AnyObject LoadInteger (AnyObject term, int integer) {
            AnyObject load = CompilerModule.CreateTerm (term.SourceLocation);
            load.SetProperty (Symbols.ParserTermName, CompilerLoadInteger);

            load.SetProperty (CompilerInteger, RuntimeModule.CreateInteger (integer));
            return load;
        }

        public static AnyObject LoadCodeBody (AnyObject term, AnyObject body) {
            AnyObject load = CompilerModule.CreateTerm (term.SourceLocation);
            load.SetProperty (Symbols.ParserTermName, CompilerLoadCodeBody);

            load.SetProperty (CompilerBody, body);
            return load;
        }

        public static AnyObject Pop (AnyObject term) {
            AnyObject pop = CompilerModule.CreateTerm (term.SourceLocation);
            pop.SetProperty (Symbols.ParserTermName, CompilerPop);

            return pop;
        }

        static AnyObject CheckForSourceLocation (AnyObject term) {
            if (term is Term) {
                if (!term.HasSourceLocation) {
                    throw new TychoException ("term without source location: " + term);
                }
            }

            return term;
        }

        public static AnyObject Return (AnyObject expression) {
            AnyObject ret = CompilerModule.CreateTerm (expression.SourceLocation, Symbols.ParserTermName, CompilerReturn, CompilerExpression, expression);
            return ret;
        }
    }

    class ByteCodeOutput {
        public List<ByteCode> ByteCodes { get; private set; }
        public List<AnyObject> _constants;

        public ByteCodeOutput () {
            ByteCodes = new List<ByteCode> ();
            _constants = new List<AnyObject> ();
        }

        public int AddConstant (AnyObject constant) {
            _constants.Add (constant);
            return _constants.Count - 1;
        }

        public ByteCode [] GetCode () {
            return ByteCodes.ToArray ();
        }

        public AnyObject [] GetConstants () {
            return _constants.ToArray ();
        }
    }
}
