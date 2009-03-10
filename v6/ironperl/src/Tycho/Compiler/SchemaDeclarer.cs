using System;
using System.Collections.Generic;
using Tycho.Runtime;

namespace Tycho.Compiler {
    class TermWalker<T> {
        private TermTransform<T> Transformer;

        public TermWalker () {
            Transformer = new TermTransform<T> ();
            Transformer.Default = (context, term) => term.Term;
        }

        public void AddWalker (Symbol termName, Action<T, TermWrapper> walker) {
            Transformer.AddTransform (termName, (context, term) => {
                walker (context, term);
                return term.Term;
            });
        }

        public void Walk (T context, AnyObject term) {
            Transformer.Transform (context, term);
        }
    }

    class SchemaDeclarationContext {
        public ExpressionContext DeclarationContext { get; private set; }
        public bool IsAssignment { get; private set; }
        public bool IsReadonlyAssignment { get; private set; }

        public SchemaDeclarationContext (ExpressionContext declarationContext, bool isAssignment, bool isReadonlyAssignment) {
            DeclarationContext = declarationContext;
            IsAssignment = isAssignment;
            IsReadonlyAssignment = isReadonlyAssignment;
        }
    }

    class SchemaDeclarer {
        private TermWalker<SchemaDeclarationContext> SchemaWalker;
        private TermWalker<SchemaDeclarationContext> StructureSchemaWalker;
        private CompilationErrors Errors;

        public SchemaDeclarer (CompilationErrors errors) {
            Errors = errors;

            SchemaWalker = BuildSchemaWalker ();
            StructureSchemaWalker = BuildStructureSchemaWalker ();
        }

        public void Declare (bool assignment, bool isReadonlyAssignment, ExpressionContext declarationContext, AnyObject term) {
            SchemaWalker.Walk (new SchemaDeclarationContext (declarationContext, assignment, isReadonlyAssignment), term);
        }

        private void DeclareSchemaVariable (SchemaDeclarationContext context, AnyObject term, Symbol symbol) {
            if (symbol.Name == ExpressionCompiler.VoidVariable) {
            } else if (context.IsAssignment) {
                if (context.IsReadonlyAssignment) {
                    context.DeclarationContext.Scope.DeclareVariableReadonly (symbol, term.SourceLocation);
                } else {
                    context.DeclarationContext.Scope.DeclareVariableTentative (symbol, term.SourceLocation);
                }
            } else if (symbol == ExpressionCompiler.SelfSymbol) {
                context.DeclarationContext.Scope.DeclareVariableOverride (symbol, term.SourceLocation);
            } else if (context.DeclarationContext != null) {
                context.DeclarationContext.Scope.DeclareVariable (symbol, term.SourceLocation);
            } else {
                Errors.CreateErrorTerm ("schemas cannot declare variables, use '_' variable instead", term);
            }
        }

        TermWalker<SchemaDeclarationContext> BuildStructureSchemaWalker () {
            TermWalker<SchemaDeclarationContext> transform = new TermWalker<SchemaDeclarationContext> ();

            transform.AddWalker (Symbols.ParserIdentifier, (context, term) => {
                Symbol symbol = context.DeclarationContext.ModuleMap.ExpectSymbol (term.Term);

                DeclareSchemaVariable (context, term.Term, symbol);
            });
            transform.AddWalker (ExpressionCompiler.FieldSymbol, (context, term) => {
                SchemaWalker.Walk (context, term [ExpressionCompiler.ValueSymbol]);
            });

            return transform;
        }

        TermWalker<SchemaDeclarationContext> BuildSchemaWalker () {
            TermWalker<SchemaDeclarationContext> transform = new TermWalker<SchemaDeclarationContext> ();

            transform.AddWalker (ExpressionCompiler.StructureSymbol, (context, term) => {
                foreach (AnyObject field in term [ExpressionCompiler.FieldsSymbol]) {
                    StructureSchemaWalker.Walk (context, field);
                }
            });

            transform.AddWalker (ExpressionCompiler.ListSymbol, (context, term) => {
                foreach (AnyObject item in term [ExpressionCompiler.ItemsSymbol]) {
                    SchemaWalker.Walk (context, item);
                }
            });

            transform.AddWalker (ExpressionCompiler.MatchSymbol, (context, term) => {
                Symbol variable = context.DeclarationContext.ModuleMap.ExpectSymbol (term [ExpressionCompiler.VariableSymbol]);
                AnyObject schema = term [ExpressionCompiler.SchemaSymbol];

                if (!schema.HasProperty (Symbols.ParserIdentifier)) {
                    SchemaWalker.Walk (context, schema);
                }

                if (context.IsAssignment) {
                    if (context.IsReadonlyAssignment) {
                        context.DeclarationContext.Scope.DeclareVariableReadonly (variable, term.Term.SourceLocation);
                    } else {
                        context.DeclarationContext.Scope.DeclareVariableTentative (variable, term.Term.SourceLocation);
                    }
                } else {
                    context.DeclarationContext.Scope.DeclareVariable (variable, term.Term.SourceLocation);
                }
            });

            transform.AddWalker (Symbols.ParserIdentifier, (context, term) => {
                Symbol symbol = context.DeclarationContext.ModuleMap.ExpectSymbol (term.Term);

                DeclareSchemaVariable (context, term.Term, symbol);
            });

            return transform;
        }
    }
}