using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Parser;
using Tycho.Parser.Tokens;
using IronPerl;

namespace Tycho.Compiler {
    class ExpressionCompiler {
        TermTransform<ExpressionContext> ExpressionTransform;
        private ModuleDeclarer ModuleDeclarer;
        private SchemaDeclarer SchemaDeclarer;
        private SchemaCompiler SchemaCompiler;
        TermTransform<CollectionExpressionContext> StructureTransform;
        TermTransform<CollectionExpressionContext> ObjectTransform;
        public FrameSymbolScope ModuleScope { get; set; }
        public ModuleMap ModuleMap { get; private set; }
        public CompilationErrors Errors { get; private set; }

        public static Symbol AliasSymbol = Namespaces.Parser.Get ("alias"),
            ArgumentsSymbol = Namespaces.Parser.Get ("arguments"),
            AdditionalArgumentsSymbol = Namespaces.Parser.Get ("additional-arguments"),
            AssignmentSymbol = Namespaces.Parser.Get ("assignment"),
            ReadonlyAssignmentSymbol = Namespaces.Parser.Get ("readonly-assignment"),
            ExportedAssignmentSymbol = Namespaces.Parser.Get ("exported-assignment"),
            BodySymbol = Namespaces.Parser.Get ("body"),
            CaseSymbol = Namespaces.Parser.Get ("case"),
            ClosureSymbol = Namespaces.Parser.Get ("closure"),
            ConditionSymbol = Namespaces.Parser.Get ("condition"),
            ElseSymbol = Namespaces.Parser.Get ("else"),
            EqualsSymbol = Namespaces.Parser.Get ("equals"),
            EtcSymbol = Namespaces.Parser.Get ("etc"),
            ExpressionSymbol = Namespaces.Parser.Get ("expression"),
            NewSymbol = Namespaces.Parser.Get ("new"),
            ConstructorSymbol = Namespaces.Parser.Get ("constructor"),
            FieldSymbol = Namespaces.Parser.Get ("field"),
            MethodSymbol = Namespaces.Parser.Get ("method"),
            GetterSymbol = Namespaces.Parser.Get ("getter"),
            SetterSymbol = Namespaces.Parser.Get ("setter"),
            MembersSymbol = Namespaces.Parser.Get ("members"),
            FieldsSymbol = Namespaces.Parser.Get ("fields"),
            FunctionSymbol = Namespaces.Parser.Get ("function"),
            FunctionCallSymbol = Namespaces.Parser.Get ("function-call"),
            GreaterThanSymbol = Namespaces.Parser.Get ("greater-than"),
            GreaterThanEqualToSymbol = Namespaces.Parser.Get ("greater-than-equal-to"),
            IfSymbol = Namespaces.Parser.Get ("if"),
            IndexSymbol = Namespaces.Parser.Get ("index"),
            PropertyGetSymbol = Namespaces.Parser.Get ("property-get"),
            ItemsSymbol = Namespaces.Parser.Get ("items"),
            LeftSymbol = Namespaces.Parser.Get ("left"),
            LessThanSymbol = Namespaces.Parser.Get ("less-than"),
            LessThanEqualToSymbol = Namespaces.Parser.Get ("less-than-equal-to"),
            ListSymbol = Namespaces.Parser.Get ("list"),
            LoopSymbol = Namespaces.Parser.Get ("loop"),
            MatchSymbol = Namespaces.Parser.Get ("match"),
            MethodCallSymbol = Namespaces.Parser.Get ("method-call"),
            NameSymbol = Namespaces.Parser.Get ("name"),
            NamespaceImportSymbol = Namespaces.Parser.Get ("namespace-import"),
            SpecializeObjectSymbol = Namespaces.Parser.Get ("specialize-object"),
            ObjectSymbol = Namespaces.Parser.Get ("object"),
            ParametersSymbol = Namespaces.Parser.Get ("parameters"),
            PrintSymbol = Namespaces.Parser.Get ("print"),
            PropertySymbol = Namespaces.Parser.Get ("property"),
            AccessorsSymbol = Namespaces.Parser.Get ("accessors"),
            PropertyAssignmentSymbol = Namespaces.Parser.Get ("property-assignment"),
            QuoteSymbol = Namespaces.Parser.Get ("quote"),
            RightSymbol = Namespaces.Parser.Get ("right"),
            SchemaSymbol = Namespaces.Parser.Get ("schema"),
            SetSymbol = Namespaces.Parser.Get ("set"),
            SourceSymbol = Namespaces.Parser.Get ("source"),
            StructureSymbol = Namespaces.Parser.Get ("structure"),
            SubExpressionSymbol = Namespaces.Parser.Get ("sub-expression"),
            SubExpressionBlockSymbol = Namespaces.Parser.Get ("sub-expression-block"),
            SwitchSymbol = Namespaces.Parser.Get ("switch"),
            SymbolSymbol = Namespaces.Parser.Get ("symbol"),
            TargetSymbol = Namespaces.Parser.Get ("target"),
            ThenSymbol = Namespaces.Parser.Get ("then"),
            ToPowerSymbol = Namespaces.Parser.Get ("to-power"),
            UnquoteSymbol = Namespaces.Parser.Get ("unquote"),
            ValueSymbol = Namespaces.Parser.Get ("value"),
            VariableSymbol = Namespaces.Parser.Get ("variable"),
            WhileSymbol = Namespaces.Parser.Get ("while"),
            ImplementsSymbol = Namespaces.Parser.Get ("implements"),
            ProtocolsSymbol = Namespaces.Parser.Get ("protocols"),
            InheritSymbol = Namespaces.Parser.Get ("inherit");

        public static Symbol SelfSymbol = Namespaces.Runtime.Get ("self");

        public const string VoidVariable = "_";
        private Symbol SetterValueSymbol;

        public ExpressionCompiler (Namespace defaultNamespace) {
            ExpressionTransform = BuildExpressionTransform ();
            SchemaDeclarer = new SchemaDeclarer (Errors);
            ModuleDeclarer = new ModuleDeclarer (SchemaDeclarer);
            SchemaCompiler = new SchemaCompiler (ExpressionTransform, Errors);
            StructureTransform = BuildStructureTransform ();
            ObjectTransform = BuildObjectTransform ();
            ModuleMap = new ModuleMap (defaultNamespace);
            SetterValueSymbol = defaultNamespace.Get ("value");

            Errors = new CompilationErrors ();
        }

        public static AnyObject LoadVariable (AnyObject name, SymbolScope scope, AnyObject term) {
            return ByteCodeCompiler.Load (term, ByteCodeCompiler.LoadConstant (term, name), scope.GetFrameIndex (name, false, term.SourceLocation));
        }

        public static AnyObject LoadVoidVariable (AnyObject term) {
            return ByteCodeCompiler.LoadConstant (term, new AnySchemaObject ());
        }

        TermTransform<ExpressionContext> BuildExpressionTransform () {
            TermTransform<ExpressionContext> transform = new TermTransform<ExpressionContext> ();
            transform.PostTransform = CompilerUtilities.AssignSourceLocation;

            transform.AddTransform (ToPowerSymbol, (context, term) => {
                return BinaryOperator (context, term, Symbols.RuntimeToPower);
            });

            transform.AddTransform (Symbols.ParserIdentifier, (context, term) => {
                Symbol symbol = context.ModuleMap.ExpectSymbol (term.Term);

                return LoadVariable (symbol, context.Scope, term.Term);
            });

            transform.AddTransform (AssignmentSymbol, (context, term) => {
                return CompileAssignment (term, context, false);
            });

            transform.AddTransform (ReadonlyAssignmentSymbol, (context, term) => {
                return CompileAssignment (term, context, true);
            });

            transform.AddTransform (ExportedAssignmentSymbol, (context, term) => {
                if (context.AtModuleLevel) {
                    return CompileAssignment (term, context, true);
                } else {
                    return Errors.CreateErrorTerm ("export can only be used at module level", term.Term);
                }
            });

            transform.AddTransform (PropertyGetSymbol, (context, term) => {
                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    ExpressionTransform.Transform (context, term [ExpressionSymbol]),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeGetProperty),
                    CompileSymbolExpression (context, term [NameSymbol]));
            });

            transform.AddTransform (StructureSymbol, (context, term) => {
                CollectionExpressionContext fieldContext = new CollectionExpressionContext (context, new List<AnyObject> ());
                foreach (AnyObject field in term [FieldsSymbol]) {
                    StructureTransform.Transform (fieldContext, field);
                }

                return CompileStructure (context.Scope, term.Term, fieldContext.Items);
            });

            transform.AddTransform (SpecializeObjectSymbol, (context, term) => {
                CollectionExpressionContext membersContext = new CollectionExpressionContext (context, new List<AnyObject> ());
                membersContext.Items.Add (ExpressionTransform.Transform (context, term [ObjectSymbol]));
                foreach (AnyObject field in term [MembersSymbol]) {
                    ObjectTransform.Transform (membersContext, field);
                }

                AnyObject loadObject = LoadVariable (Symbols.RuntimeObject, context.Scope, term.Term);
                return ByteCodeCompiler.MethodCall (term.Term, loadObject, ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeSpecialize), membersContext.Items.ToArray ());
            });

            transform.AddTransform (Symbols.ParserInteger, (context, term) => {
                return ByteCodeCompiler.LoadInteger (term.Term, term [Symbols.ParserInteger].ExpectValue<int> ());
            });

            transform.AddTransform (Symbols.ParserString, (context, term) => {
                return ByteCodeCompiler.LoadConstant (term.Term, term [Symbols.ParserString].ExpectValue<string> ());
            });

            transform.AddTransform (Symbols.ParserInterpolatedString, (context, term) => {
                AnyObject[] values = term[Symbols.ParserValues].Select (v => ExpressionTransform.Transform (context, v)).ToArray ();
                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeConcatenateToString, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    values);
            });

            transform.AddTransform (Symbols.ParserReal, (context, term) => {
                return ByteCodeCompiler.LoadConstant (term.Term, term [Symbols.ParserReal].ExpectValue<double> ());
            });

            transform.AddTransform (FunctionCallSymbol, (context, term) => {
                bool listArguments;
                AnyObject [] arguments = OperationArguments (context, GetAllArgumentsForCall (term), out listArguments);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    listArguments,
                    ExpressionTransform.Transform (context, term [FunctionSymbol]),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    arguments);
            });

            transform.AddTransform (MethodCallSymbol, (context, term) => {
                bool listArguments;
                AnyObject [] arguments = OperationArguments (context, GetAllArgumentsForCall (term), out listArguments);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    listArguments,
                    ExpressionTransform.Transform (context, term [ObjectSymbol]),
                    CompileSymbolExpression (context, term [NameSymbol]),
                    arguments);
            });

            transform.AddTransform (ClosureSymbol, (context, term) => {
                return CompileClosure (term.Term, term [ParametersSymbol], term [BodySymbol], context);
            });

            transform.AddTransform (IfSymbol, (context, term) => {
                AnyObject [] arguments;

                if (!term.Has (ElseSymbol)) {
                    arguments = new AnyObject [2];
                    arguments [0] = ExpressionTransform.Transform (context, term [ConditionSymbol]);
                    arguments [1] = CompileClosure (term.Term, CompilerModule.CreateTermList (), term [ThenSymbol], context);
                } else {
                    arguments = new AnyObject [3];
                    arguments [0] = ExpressionTransform.Transform (context, term [ConditionSymbol]);
                    arguments [1] = CompileClosure (term.Term, CompilerModule.CreateTermList (), term [ThenSymbol], context);
                    arguments [2] = CompileClosure (term.Term, CompilerModule.CreateTermList (), term [ElseSymbol], context);
                }

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeIf, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    arguments);
            });

            transform.AddTransform (WhileSymbol, (context, term) => {
                AnyObject [] arguments = new AnyObject [2];

                arguments [0] = CompileClosure (term.Term, CompilerModule.CreateTermList (), term [ConditionSymbol], context);
                arguments [1] = CompileClosure (term.Term, CompilerModule.CreateTermList (), term [LoopSymbol], context);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeWhile, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    arguments);
            });

            transform.AddTransform (ListSymbol, (context, term) => {
                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeList, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeNew),
                    term [ItemsSymbol].Select (item => ExpressionTransform.Transform (context, item)).ToArray ());
            });

            transform.AddTransform (SymbolSymbol, (context, term) => {
                return ByteCodeCompiler.LoadConstant (term.Term, context.ModuleMap.ExpectSymbol (term [SymbolSymbol]));
            });

            transform.AddTransform (NamespaceImportSymbol, (context, term) => {
                ModuleMap map = new ModuleMap (Namespaces.Root);
                Symbol target = map.ExpectSymbol (term [TargetSymbol]);
                Symbol alias = map.ExpectSymbol (term [AliasSymbol]);
                context.ModuleMap.Add (alias.Namespace.GetNamespace (alias.Name), target.Namespace.GetNamespace (target.Name));
                return LoadVariable (Symbols.RuntimeNull, context.Scope, term.Term);
            });

            /*transform.AddTransform (QuoteSymbol, (context, term) => {
                AnyObject generatedModule = CompilerModule.CreateTermList (context.ModuleMap.Default.Path.Select (p => (AnyObject) p).ToArray ());
                return CompileQuote (term [ExpressionSymbol], generatedModule, context);
            });*/

            transform.AddTransform (SubExpressionSymbol, (context, term) => {
                return ExpressionTransform.Transform (context, term [ExpressionSymbol]);
            });

            transform.AddTransform (SubExpressionBlockSymbol, (context, term) => {
                return CompileStatements (context, term [ExpressionSymbol], term.Term);
            });

            transform.AddTransform (PrintSymbol, (context, term) => {
                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimePrint, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    ExpressionTransform.Transform (context, term [ExpressionSymbol]));
            });

            transform.AddTransform (IndexSymbol, (context, term) => {
                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    ExpressionTransform.Transform (context, term [ExpressionSymbol]),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeIndexGet),
                    term [IndexSymbol].Select (index => ExpressionTransform.Transform (context, index)).ToArray ());
            });

            transform.AddTransform (LessThanSymbol, (context, term) => {
                return BinaryOperator (context, term, Symbols.RuntimeLessThan);
            });

            transform.AddTransform (GreaterThanSymbol, (context, term) => {
                return BinaryOperator (context, term, Symbols.RuntimeGreaterThan);
            });

            transform.AddTransform (LessThanEqualToSymbol, (context, term) => {
                return BinaryOperator (context, term, Symbols.RuntimeLessThanEqualTo);
            });

            transform.AddTransform (GreaterThanEqualToSymbol, (context, term) => {
                return BinaryOperator (context, term, Symbols.RuntimeGreaterThanEqualTo);
            });

            transform.AddTransform (EqualsSymbol, (context, term) => {
                return BinaryOperator (context, term, Symbols.RuntimeEquals);
            });

            transform.AddTransform (SetSymbol, (context, term) => {
                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeSet, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeNew),
                    term [ItemsSymbol].Select (item => ExpressionTransform.Transform (context, item)).ToArray ());
            });

            transform.AddTransform (FieldSymbol, (context, term) => {
                return CompileField (context, term, false);
            });

            transform.AddTransform (PropertySymbol, (context, term) => {
                AnyObject propertyNameTerm = term [NameSymbol];
                AnyObject propertyName = context.ModuleMap.ExpectSymbol (propertyNameTerm);

                AnyObject getter = null, setter = null;
                foreach (AnyObject accessor in term [AccessorsSymbol]) {
                    AnyObject termName = accessor [Symbols.ParserTermName];

                    if (termName == GetterSymbol) {
                        if (getter != null) {
                            throw new CompilationException ("getter already defined", accessor);
                        }
                        getter = CompileGetter (propertyNameTerm, propertyName, accessor, accessor [BodySymbol], context);
                    } else if (termName == SetterSymbol) {
                        if (setter != null) {
                            throw new CompilationException ("setter already defined", accessor);
                        }
                        setter = CompileSetter (propertyNameTerm, propertyName, accessor, accessor [BodySymbol], context);
                    }
                }

                if (getter != null && setter != null) {
                    return CompilerModule.CreateTermList (new[] { getter, setter });
                } else if (setter != null) {
                    return setter;
                } else if (getter != null) {
                    return getter;
                } else {
                    throw new CompilationException ("must define at least a setter or getter", term.Term);
                }
            });

            transform.AddTransform (MethodSymbol, (context, term) => {
                AnyObject methodNameTerm = term [NameSymbol];
                AnyObject methodName = context.ModuleMap.ExpectSymbol (methodNameTerm);
                AnyObject parameters = CompilerModule.CreateTermList (term [ParametersSymbol]);
                parameters.Insert (0, CompilerUtilities.SymbolToIdentifier (SelfSymbol, term.Term.SourceLocation));
                AnyObject method = CompileClosure (term.Term, parameters, term [BodySymbol], context);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (SelfSymbol, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeAddMethod),
                    ByteCodeCompiler.LoadConstant (term.Term, methodName),
                    method);
            });

            transform.AddTransform (ImplementsSymbol, (context, term) => {
                AnyObject [] protocols = term [ProtocolsSymbol].Select (p => ExpressionTransform.Transform (context, p)).ToArray ();

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (SelfSymbol, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeAddProtocols),
                    protocols);
            });

            transform.AddTransform (InheritSymbol, (context, term) => {
                AnyObject constructor = ExpressionTransform.Transform (context, term[ConstructorSymbol]);
                List<AnyObject> arguments = term [ArgumentsSymbol].Select (a => ExpressionTransform.Transform (context, a)).ToList ();
                arguments.Insert (0, LoadVariable (SelfSymbol, context.Scope, term.Term));

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    constructor,
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeConstruct),
                    arguments.ToArray ());
            });

            transform.AddTransform (ConstructorSymbol, (context, term) => {
                AnyObject parameters = CompilerModule.CreateTermList (term [ParametersSymbol]);
                parameters.Insert (0, CompilerUtilities.SymbolToIdentifier (SelfSymbol, term.Term.SourceLocation));

                AnyObject constructorClosure = CompileClosure (term.Term, parameters, term [BodySymbol], context);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeConstructor, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeNew),
                    constructorClosure);
            });

            transform.AddTransform (NewSymbol, (context, term) => {
                AnyObject [] arguments = term [ArgumentsSymbol].Select (arg => ExpressionTransform.Transform (context, arg)).ToArray ();
                AnyObject constructor = ExpressionTransform.Transform (context, term[ConstructorSymbol]);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    constructor,
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeNew),
                    arguments);
            });

            transform.AddTransform (SwitchSymbol, (context, term) => {
                AnyObject cases = term [CaseSymbol];
                AnyObject [] arguments = new AnyObject [cases.Count + 1];
                arguments [0] = ExpressionTransform.Transform (context, term [ExpressionSymbol]);

                int n = 1;
                foreach (AnyObject c in term [CaseSymbol]) {
                    AnyObject schema = c.GetProperty (SchemaSymbol);
                    AnyObject body = c.GetProperty (ExpressionSymbol);
                    arguments [n] = CompileClosure (term.Term, CompilerModule.CreateTermList (new AnyObject [] { schema }), body, context);
                    n++;
                }

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (Symbols.RuntimeSwitch, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    arguments);
            });

            transform.AddTransform (SchemaSymbol, (context, term) => {
                return SchemaCompiler.Compile (new SchemaContext (false, context, context.Scope, false), term [SchemaSymbol]);
            });

            return transform;
        }

        private AnyObject CompileAssignment (TermWrapper term, ExpressionContext context, bool isReadonly) {
            if (term [TargetSymbol].GetProperty (Symbols.ParserTermName) == Symbols.ParserIdentifier) {
                return CompileSimpleAssignment (term, context, isReadonly);
            }

            SchemaContext schemaContext = new SchemaContext (true, context, context.Scope, true);

            if (!context.AtModuleLevel) {
                SchemaDeclarer.Declare (true, isReadonly, context, term[TargetSymbol]);
            }

            AnyObject matchCall = ByteCodeCompiler.MethodCall (
                term.Term,
                LoadVariable (Symbols.RuntimeMatchAssignment, context.Scope, term.Term),
                ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                SchemaCompiler.Compile (schemaContext, term [TargetSymbol]),
                ByteCodeCompiler.LoadStackFrame (term.Term),
                ExpressionTransform.Transform (context, term [SourceSymbol]));

            // if there are property assignments (properties used in the LHS of the assignment)
            // we first put the values in the stack frame, then we need to set the properties
            // this block runs statements to set the properties using the temp values
            if (schemaContext.ComplexAssignments.Count > 0) {
                AnyObject statements = CompilerModule.CreateTermList ();

                statements.Add (matchCall);

                CompileComplexAssignments (context.Scope, schemaContext.ComplexAssignments, statements);

                return statements;
            } else {
                return matchCall;
            }
        }

        private AnyObject CompileSimpleAssignment (TermWrapper term, ExpressionContext context, bool isReadonly) {
            Symbol symbol = context.ModuleMap.ExpectSymbol (term [TargetSymbol]);
            int frameIndex;
            if (context.AtModuleLevel) {
                frameIndex = context.Scope.GetFrameIndex (symbol, false, term [TargetSymbol].SourceLocation);
            } else {
                if (isReadonly) {
                    frameIndex = context.Scope.DeclareVariableReadonly (symbol, term [TargetSymbol].SourceLocation);
                } else {
                    frameIndex = context.Scope.DeclareVariableTentative (symbol, term[TargetSymbol].SourceLocation);
                }
            }
            return ByteCodeCompiler.Store (term.Term, ExpressionTransform.Transform (context, term [SourceSymbol]), ByteCodeCompiler.LoadConstant (term.Term, symbol), frameIndex);
        }

        private AnyObject CompileField (ExpressionContext context, TermWrapper term, bool overrideField) {
            AnyObject statements = CompilerModule.CreateTermList ();

            AnyObject names = term [NameSymbol];
            AnyObject values = term [ValueSymbol];
            for (int n = 0; n < names.Count; n++) {
                AnyObject nameTerm = names [n];
                AnyObject name = ByteCodeCompiler.LoadConstant (nameTerm, ModuleMap.ExpectSymbol (nameTerm));
                AnyObject value = ExpressionTransform.Transform (context, values [n]);

                AnyObject addFieldCall = ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (SelfSymbol, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeAddField),
                    name,
                    value,
                    ByteCodeCompiler.LoadConstant (term.Term, overrideField));

                statements.Add (addFieldCall);
            }

            return statements;
        }

        private AnyObject CompileProperty (ExpressionContext context, TermWrapper term, bool overrideField) {
            AnyObject statements = CompilerModule.CreateTermList ();

            AnyObject names = term [NameSymbol];
            AnyObject values = term [ValueSymbol];
            for (int n = 0; n < names.Count; n++) {
                AnyObject nameTerm = names [n];
                AnyObject name = ByteCodeCompiler.LoadConstant (nameTerm, ModuleMap.ExpectSymbol (nameTerm));
                AnyObject value = ExpressionTransform.Transform (context, values [n]);

                AnyObject addFieldCall = ByteCodeCompiler.MethodCall (
                    term.Term,
                    LoadVariable (SelfSymbol, context.Scope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeAddField),
                    name,
                    value,
                    ByteCodeCompiler.LoadConstant (term.Term, overrideField));

                statements.Add (addFieldCall);
            }

            return statements;
        }

        private AnyObject GetAllArgumentsForCall (TermWrapper term) {
            AnyObject allArguments = CompilerModule.CreateList (term[ArgumentsSymbol]);
            foreach (var arg in term [AdditionalArgumentsSymbol]) {
                allArguments.Add (arg);
            }
            return allArguments;
        }

        private AnyObject CompileStructure (FrameSymbolScope scope, AnyObject term, IEnumerable<AnyObject> fields) {
            AnyObject loadStructure = LoadVariable (Symbols.RuntimeStructure, scope, term);
            AnyObject loadNew = ByteCodeCompiler.LoadConstant (term, Symbols.RuntimeNew);
            return ByteCodeCompiler.MethodCall (term, loadStructure, loadNew, fields.ToArray ());
        }

        /// <summary>
        /// [object.prop1, object.prop2] = [3, 4]
        /// 
        /// becomes:
        /// 
        /// whole-result = [temp1, temp2] = [3, 4]
        /// object.prop1 = temp1
        /// object.prop2 = temp2
        /// whole-result
        /// 
        /// For assignment to properties, we build a number of temporary variables to hold the
        /// values as they come out of the match expression. The match expression can only 
        /// set variables on the stack not properties on an object. After we've set the variables
        /// we need to set the properties using those values. This method takes a list of
        /// temp variable details (in the form of SchemaPropertyAssignment objects) and
        /// generates code to assign those variables to their corresponding properties.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="complexAssignments"></param>
        /// <param name="statements"></param>
        private void CompileComplexAssignments (FrameSymbolScope scope, List<ComplexAssignment> complexAssignments, AnyObject statements) {
            foreach (ComplexAssignment property in complexAssignments) {
                statements.Add (property.CompileAssignment ());

                // we're not interested in the return value of the property assignment, we're only
                // interested in the return value of the whole original assignment, so we'll
                // pop this one off.
                statements.Add (ByteCodeCompiler.Pop (property.Term));
            }
        }

        AnyObject [] OperationArguments (ExpressionContext context, AnyObject argumentList, out bool listArguments) {
            IList<AnyObject> arguments = argumentList.Expect<ListObject> ().Items;
            AnyObject [] argArray;
            if (arguments.Count >= 2
                && arguments [arguments.Count - 1].GetProperty (Symbols.ParserTermName) == EtcSymbol) {

                argArray = new AnyObject [arguments.Count - 1];
                for (int i = 0; i < arguments.Count - 1; i++) {
                    argArray [i] = ExpressionTransform.Transform (context, arguments [i]);
                }
                listArguments = true;
            } else {
                argArray = argumentList.Select (arg => ExpressionTransform.Transform (context, arg)).ToArray ();
                listArguments = false;
            }

            return argArray;
        }

        AnyObject BinaryOperator (ExpressionContext context, TermWrapper term, AnyObject methodName) {
            return ByteCodeCompiler.MethodCall (
                term.Term,
                ExpressionTransform.Transform (context, term [LeftSymbol]),
                ByteCodeCompiler.LoadConstant (term.Term, methodName),
                ExpressionTransform.Transform (context, term [RightSymbol]));
        }

        AnyObject Return (AnyObject term) {
            return ByteCodeCompiler.Return (term);
        }

        bool HasVariableParameters (IList<AnyObject> parameters) {
            if (parameters.Count >= 2
                && parameters [parameters.Count - 1].GetProperty (Symbols.ParserTermName) == EtcSymbol) {

                AnyObject lastParameter = parameters [parameters.Count - 2];
                AnyObject lastParameterTermName = lastParameter.GetProperty (Symbols.ParserTermName);

                if (lastParameterTermName != Symbols.ParserIdentifier && lastParameterTermName != MatchSymbol) {
                    throw new CompilationException ("last parameter must be match", lastParameter);
                }

                return true;
            } else
                return false;
        }

        AnyObject CompileClosure (AnyObject term, AnyObject parameterList, AnyObject body, ExpressionContext context) {
            ExpressionContext nestedContext = context.CreateNestedScope ();
            SchemaContext schemaContext = new SchemaContext (false, nestedContext, context.Scope, true);
            IList<AnyObject> parameters = parameterList.Expect<ListObject> ().Items;

            bool hasVarParams = HasVariableParameters (parameters);
            int parametersCount = hasVarParams ? parameters.Count - 1 : parameters.Count;
            AnyObject [] schemaArguments = new AnyObject [parametersCount + 1];
            schemaArguments [0] = ByteCodeCompiler.LoadConstant (term, RuntimeModule.CreateBoolean (hasVarParams));
            for (int i = 1; i <= parametersCount; i++) {
                SchemaDeclarer.Declare (false, false, nestedContext, parameters [i - 1]);
                schemaArguments [i] = SchemaCompiler.Compile (schemaContext, parameters [i - 1]);
            }

            AnyObject compiledBody = ExpressionTransform.Transform (nestedContext, body);

            return ByteCodeCompiler.MethodCall (
                term,
                LoadVariable (Symbols.RuntimeClosure, context.Scope, term),
                ByteCodeCompiler.LoadConstant (term, Symbols.RuntimeNew),
                ByteCodeCompiler.LoadCodeBody (term, compiledBody),
                ByteCodeCompiler.MethodCall (
                    term,
                    LoadVariable (Symbols.RuntimeClosure, context.Scope, term),
                    ByteCodeCompiler.LoadConstant (term, Symbols.RuntimeNewParametersSchema),
                    schemaArguments),
                ByteCodeCompiler.LoadStackFrame (term),
                ByteCodeCompiler.LoadConstant (term, term.SourceLocation.ToTerm ()));
        }

        /// <summary>
        /// Compiles a list of statements, adding a return statement to return the last
        /// statement. If there are no statements in the list, a return statement is
        /// added to return null.
        /// </summary>
        /// <param name="statements"></param>
        /// <param name="sourceLocationTerm"></param>
        /// <returns></returns>
        public AnyObject CompileStatements (ExpressionContext context, AnyObject statements, AnyObject sourceLocationTerm) {
            AnyObject newStatements = CompilerModule.CreateTermList ();

            if (statements.Count > 0) {
                foreach (AnyObject statement in statements) {
                    newStatements.Add (ExpressionTransform.Transform (context, statement));
                }
            } else {
                newStatements.Add (ExpressionTransform.Transform (context, Null (sourceLocationTerm.SourceLocation)));
            }

            return newStatements;
        }

        TermTransform<CollectionExpressionContext> BuildStructureTransform () {
            TermTransform<CollectionExpressionContext> transform = new TermTransform<CollectionExpressionContext> ();

            transform.AddTransform (Symbols.ParserIdentifier, (context, term) => {
                context.Items.Add (ByteCodeCompiler.LoadConstant (term.Term, context.Context.ModuleMap.ExpectSymbol (term.Term)));
                context.Items.Add (LoadVariable (Symbols.RuntimeNull, context.Context.Scope, term.Term));
                return null;
            });
            transform.AddTransform (FieldSymbol, (context, term) => {
                context.Items.Add (ByteCodeCompiler.LoadConstant (term.Term, context.Context.ModuleMap.ExpectSymbol (term [NameSymbol])));
                context.Items.Add (ExpressionTransform.Transform (context.Context, term [ValueSymbol]));

                return null;
            });

            return transform;
        }

        TermTransform<CollectionExpressionContext> BuildObjectTransform() {
            TermTransform<CollectionExpressionContext> transform = new TermTransform<CollectionExpressionContext> ();

            transform.AddTransform (MethodSymbol, (context, term) => {
                AnyObject methodNameTerm = term [NameSymbol];
                AnyObject methodName = context.Context.ModuleMap.ExpectSymbol (methodNameTerm);
                AnyObject parameters = CompilerModule.CreateTermList (term [ParametersSymbol]);
                parameters.Insert (0, CompilerUtilities.SymbolToIdentifier (SelfSymbol, term.Term.SourceLocation));
                AnyObject method = CompileClosure (term.Term, parameters, term [BodySymbol], context.Context);

                AnyObject [] fields = new [] {
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeName),
                    ByteCodeCompiler.LoadConstant (methodNameTerm, methodName),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeMethod),
                    method};
                AnyObject structure = CompileStructure (context.Context.Scope, term.Term, fields);

                context.Items.Add (structure);

                return null;
            });

            transform.AddTransform (FieldSymbol, (context, term) => {
                AnyObject fieldNameTerm = term [NameSymbol];
                AnyObject fieldName = context.Context.ModuleMap.ExpectSymbol (fieldNameTerm);

                AnyObject [] fields = new [] {
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeName),
                    ByteCodeCompiler.LoadConstant (fieldNameTerm, fieldName),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeField),
                    ExpressionTransform.Transform (context.Context, term [ValueSymbol])};
                AnyObject structure = CompileStructure (context.Context.Scope, term.Term, fields);
                context.Items.Add (structure);

                return null;
            });

            return transform;
        }

        AnyObject CompileGetter (AnyObject propertyNameTerm, AnyObject propertyName, AnyObject getter, AnyObject getterBody, ExpressionContext context) {
            AnyObject parameters = CompilerModule.CreateList (new [] { CompilerUtilities.SymbolToIdentifier (SelfSymbol, getterBody.SourceLocation) });

            return ByteCodeCompiler.MethodCall (
                propertyNameTerm,
                LoadVariable (SelfSymbol, context.Scope, propertyNameTerm),
                ByteCodeCompiler.LoadConstant (propertyNameTerm, Symbols.RuntimeAddPropertyGetter),
                ByteCodeCompiler.LoadConstant (propertyNameTerm, propertyName),
                CompileClosure (propertyNameTerm, parameters, getterBody, context),
                ByteCodeCompiler.LoadConstant (propertyNameTerm, false));
        }

        AnyObject CompileSetter (AnyObject propertyNameTerm, AnyObject propertyName, AnyObject setter, AnyObject setterBody, ExpressionContext context) {
            AnyObject parameters = CompilerModule.CreateList (new [] {
                CompilerUtilities.SymbolToIdentifier (SelfSymbol, setterBody.SourceLocation),
                CompilerUtilities.SymbolToIdentifier (SetterValueSymbol, setterBody.SourceLocation)});

            return ByteCodeCompiler.MethodCall (
                propertyNameTerm,
                LoadVariable (SelfSymbol, context.Scope, propertyNameTerm),
                ByteCodeCompiler.LoadConstant (propertyNameTerm, Symbols.RuntimeAddPropertySetter),
                ByteCodeCompiler.LoadConstant (propertyNameTerm, propertyName),
                CompileClosure (propertyNameTerm, parameters, setterBody, context),
                ByteCodeCompiler.LoadConstant (propertyNameTerm, false));
        }

        public AnyObject Compile (AnyObject term, FrameSymbolScope scope, bool atModuleLevel) {
            if (atModuleLevel) {
                ModuleDeclarer.Declare (new ExpressionContext (scope, ModuleMap, true), ModuleScope, term);
            }
            return ExpressionTransform.Transform (new ExpressionContext (scope, ModuleMap, atModuleLevel), term);
        }

        AnyObject CompileSymbolExpression (ExpressionContext context, AnyObject term) {
            if (term.HasProperty (Symbols.ParserIdentifier)) {
                return ByteCodeCompiler.LoadConstant (term, context.ModuleMap.ExpectSymbol (term));
            } else {
                return ExpressionTransform.Transform (context, term);
            }
        }
        /*
        class UnquotedExpressions {
            public readonly List<AnyObject> Expressions;

            public UnquotedExpressions () {
                Expressions = new List<AnyObject> ();
            }

            public TermGenerator UnquoteExpression (AnyObject expression) {
                Expressions.Add (expression);
                return new QuoteExpressionGenerator (Expressions.Count - 1);
            }
        }

        AnyObject CompileQuote (AnyObject term, AnyObject modulePath, ExpressionContext context) {
            TermGeneratorBuilder builder = new TermGeneratorBuilder (modulePath);
            UnquotedExpressions expressions = new UnquotedExpressions ();
            MacroOperation quotedCode = new MacroOperation (builder.Build (term, expressions.UnquoteExpression));
            AnyObject[] arguments = expressions.Expressions.Select (e => ExpressionTransform.Transform (context, e)).ToArray ();

            return ByteCodeCompiler.MethodCall (
                term,
                ByteCodeCompiler.LoadConstant (term, quotedCode),
                ByteCodeCompiler.LoadConstant (term, Symbols.RuntimeInvoke),
                arguments);
        }
        */
        AnyObject BuildSymbol (AnyObject term, SourceLocation sourceLocation) {
            AnyObject id = CompilerUtilities.SymbolToIdentifier (term.Expect<Symbol> (), sourceLocation);

            id.SourceLocation = sourceLocation;

            return id;
        }

        public static AnyObject BuildSubExpressionBlock (AnyObject term, SourceLocation sourceLocation) {
            return CompilerModule.CreateTerm (
                sourceLocation,
                Symbols.ParserTermName, SubExpressionBlockSymbol,
                ExpressionSymbol, term);
        }

        #region Terms

        public static AnyObject Null (SourceLocation sloc) {
            return CompilerUtilities.SymbolToIdentifier (Symbols.RuntimeNull, sloc);
        }

        public static AnyObject Symbol (Symbol s, SourceLocation sloc) {
            AnyObject symbol = CompilerModule.CreateTerm (sloc);
            symbol.SetProperty (Symbols.ParserTermName, SymbolSymbol);
            symbol.SetProperty (SymbolSymbol, Terms.FromSymbol (s, sloc));

            return symbol;
        }

        public static AnyObject Quote (AnyObject bodyTerm, SourceLocation sloc) {
            AnyObject quoteTerm = CompilerModule.CreateTerm (sloc);
            quoteTerm.SetProperty (Symbols.ParserTermName, QuoteSymbol);
            quoteTerm.SetProperty (ExpressionSymbol, bodyTerm);

            return quoteTerm;
        }

        #endregion
    }
}
