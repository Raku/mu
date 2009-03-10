using System.Collections.Generic;
using Tycho.Runtime;
using IronPerl;

namespace Tycho.Compiler {
    class SchemaCompiler {
        private TermTransform<SchemaContext> SchemaTransform;
        private TermTransform<ExpressionContext> ExpressionTransform;
        private TermTransform<CollectionSchemaContext> StructureSchemaTransform;
        private CompilationErrors Errors;

        public SchemaCompiler (TermTransform<ExpressionContext> expressionTransform, CompilationErrors errors) {
            ExpressionTransform = expressionTransform;
            Errors = errors;

            SchemaTransform = BuildSchemaTransform ();
            StructureSchemaTransform = BuildStructureSchemaTransform ();
        }

        public AnyObject Compile (SchemaContext context, AnyObject term) {
            return SchemaTransform.Transform (context, term);
        }

        private AnyObject CompileSchemaVariable (SchemaContext context, AnyObject term, Symbol symbol) {
            int frameIndex;

            if (symbol.Name == ExpressionCompiler.VoidVariable) {
                return ExpressionCompiler.LoadVoidVariable (term);
            } else if (context.ForDeclaration) {
                frameIndex = context.Context.Scope.GetFrameIndex (symbol, false, term.SourceLocation);

                return ByteCodeCompiler.MethodCall (
                    term,
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeMatchingSchema, context.ConstructionScope, term),
                    ByteCodeCompiler.LoadConstant (term, Symbols.RuntimeInvoke),
                    ByteCodeCompiler.LoadConstant (term, symbol),
                    ByteCodeCompiler.LoadInteger (term, frameIndex),
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeNull, context.ConstructionScope, term));
            } else {
                return Errors.CreateErrorTerm ("schemas cannot declare variables, use '_' variable instead", term);
            }
        }

        TermTransform<CollectionSchemaContext> BuildStructureSchemaTransform () {
            TermTransform<CollectionSchemaContext> transform = new TermTransform<CollectionSchemaContext> ();

            transform.AddTransform (Symbols.ParserIdentifier, (context, term) => {
                Symbol symbol = context.SchemaContext.Context.ModuleMap.ExpectSymbol (term.Term);
                context.Items.Add (ByteCodeCompiler.LoadConstant (term.Term, symbol));

                if (context.SchemaContext.ForDeclaration) {
                    context.Items.Add (CompileSchemaVariable (context.SchemaContext, term.Term, symbol));
                } else {
                    context.Items.Add (ExpressionCompiler.LoadVoidVariable (term.Term));
                }
                return null;
            });
            transform.AddTransform (ExpressionCompiler.FieldSymbol, (context, term) => {
                context.Items.Add (ByteCodeCompiler.LoadConstant (term.Term, context.SchemaContext.Context.ModuleMap.ExpectSymbol (term [ExpressionCompiler.NameSymbol])));
                context.Items.Add (SchemaTransform.Transform (context.SchemaContext, term [ExpressionCompiler.ValueSymbol]));

                return null;
            });

            return transform;
        }

        TermTransform<SchemaContext> BuildSchemaTransform () {
            TermTransform<SchemaContext> transform = new TermTransform<SchemaContext> ();
            transform.PostTransform = CompilerUtilities.AssignSourceLocation;

            transform.AddTransform (ExpressionCompiler.StructureSymbol, (context, term) => {
                AnyObject loadStructure = ExpressionCompiler.LoadVariable (Symbols.RuntimeStructure, context.ConstructionScope, term.Term);

                CollectionSchemaContext fieldContext = new CollectionSchemaContext (new List<AnyObject> (), context);
                foreach (AnyObject field in term [ExpressionCompiler.FieldsSymbol]) {
                    StructureSchemaTransform.Transform (fieldContext, field);
                }

                return ByteCodeCompiler.MethodCall (term.Term, loadStructure, ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeNew), fieldContext.Items.ToArray ());
            });

            transform.AddTransform (ExpressionCompiler.ListSymbol, (context, term) => {
                AnyObject [] items;

                items = new AnyObject [term [ExpressionCompiler.ItemsSymbol].Count];

                int n = 0;
                foreach (AnyObject item in term [ExpressionCompiler.ItemsSymbol]) {
                    items [n++] = SchemaTransform.Transform (context, item);
                }

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeList, context.ConstructionScope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeNew),
                    items);
            });

            transform.AddTransform (ExpressionCompiler.MatchSymbol, (context, term) => {
                Symbol variable = context.Context.ModuleMap.ExpectSymbol (term [ExpressionCompiler.VariableSymbol]);
                AnyObject schema = term [ExpressionCompiler.SchemaSymbol];

                AnyObject schemaExpression;

                if (schema.HasProperty (Symbols.ParserIdentifier)) {
                    Symbol schemaSymbol = context.Context.ModuleMap.ExpectSymbol (schema);
                    schemaExpression = ExpressionCompiler.LoadVariable (schemaSymbol, context.ConstructionScope, term.Term);
                } else {
                    schemaExpression = SchemaTransform.Transform (context, schema);
                }

                int frameIndex = context.Context.Scope.GetFrameIndex (variable, true, term.Term.SourceLocation);

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeMatchingSchema, context.ConstructionScope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    ByteCodeCompiler.LoadConstant (term.Term, variable),
                    ByteCodeCompiler.LoadInteger (term.Term, frameIndex),
                    schemaExpression);
            });

            transform.AddTransform (Symbols.ParserIdentifier, (context, term) => {
                Symbol symbol = context.Context.ModuleMap.ExpectSymbol (term.Term);

                return CompileSchemaVariable (context, term.Term, symbol);
            });

            transform.AddTransform (Symbols.ParserInteger, (scope, term) => {
                return ByteCodeCompiler.LoadInteger (term.Term, term [Symbols.ParserInteger].ExpectValue<int> ());
            });

            transform.AddTransform (Symbols.ParserString, (scope, term) => {
                return ByteCodeCompiler.LoadConstant (term.Term, term [Symbols.ParserString].ExpectValue<string> ());
            });

            transform.AddTransform (Symbols.ParserReal, (scope, term) => {
                return ByteCodeCompiler.LoadConstant (term.Term, term [Symbols.ParserReal].ExpectValue<double> ());
            });

            transform.AddTransform (ExpressionCompiler.PropertyAssignmentSymbol, (context, term) => {
                if (!context.Assignment) {
                    return Errors.CreateErrorTerm ("property references cannot form a schema", term.Term);
                }

                Symbol symbol = Namespaces.Parser.GetUnique ("property-assignment-temp");
                int frameIndex = context.Context.Scope.DeclareVariable (symbol, term.Term.SourceLocation);

                context.ComplexAssignments.Add (new PropertyAssignment (term[ExpressionCompiler.NameSymbol], term[ExpressionCompiler.ExpressionSymbol], ExpressionTransform, context.Context, term.Term, symbol, frameIndex));

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeMatchingSchema, context.ConstructionScope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    ByteCodeCompiler.LoadConstant (term.Term, symbol),
                    ByteCodeCompiler.LoadInteger (term.Term, frameIndex),
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeNull, context.ConstructionScope, term.Term));
            });

            transform.AddTransform (ExpressionCompiler.IndexSymbol, (context, term) => {
                if (!context.Assignment) {
                    return Errors.CreateErrorTerm ("index references cannot form a schema", term.Term);
                }

                Symbol symbol = Namespaces.Parser.GetUnique ("index-assignment-temp");
                int frameIndex = context.Context.Scope.DeclareVariable (symbol, term.Term.SourceLocation);

                context.ComplexAssignments.Add (new IndexAssignment (term [ExpressionCompiler.IndexSymbol], term [ExpressionCompiler.ExpressionSymbol], ExpressionTransform, context.Context, term.Term, symbol, frameIndex));

                return ByteCodeCompiler.MethodCall (
                    term.Term,
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeMatchingSchema, context.ConstructionScope, term.Term),
                    ByteCodeCompiler.LoadConstant (term.Term, Symbols.RuntimeInvoke),
                    ByteCodeCompiler.LoadConstant (term.Term, symbol),
                    ByteCodeCompiler.LoadInteger (term.Term, frameIndex),
                    ExpressionCompiler.LoadVariable (Symbols.RuntimeNull, context.ConstructionScope, term.Term));
            });

            return transform;
        }
    }
}
