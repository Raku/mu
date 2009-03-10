using Tycho.Runtime;

namespace Tycho.Compiler {
    class ModuleDeclarer {
        private TermWalker<ModuleDeclarationContext> AssignmentWalker;
        private SchemaDeclarer SchemaDeclarer;

        public ModuleDeclarer (SchemaDeclarer schemaDeclarer) {
            SchemaDeclarer = schemaDeclarer;
            AssignmentWalker = BuildAssignmentWalker ();
        }

        public void Declare (ExpressionContext context, FrameSymbolScope moduleScope, AnyObject term) {
            AssignmentWalker.Walk (new ModuleDeclarationContext (context, moduleScope), term);
        }

        public TermWalker<ModuleDeclarationContext> BuildAssignmentWalker () {
            var walker = new TermWalker<ModuleDeclarationContext> ();

            walker.AddWalker (ExpressionCompiler.AssignmentSymbol, (context, term) => {
                DeclareDefinition (term, context, context.Context.Scope, false);
            });

            walker.AddWalker (ExpressionCompiler.ReadonlyAssignmentSymbol, (context, term) => {
                DeclareDefinition (term, context, context.Context.Scope, true);
            });

            walker.AddWalker (ExpressionCompiler.ExportedAssignmentSymbol, (context, term) => {
                DeclareDefinition (term, context, context.ModuleScope, true);
            });

            return walker;
        }

        private void DeclareDefinition (TermWrapper term, ModuleDeclarationContext context, FrameSymbolScope scope, bool isReadonly) {
            AnyObject target = term[ExpressionCompiler.TargetSymbol];
            if (target.GetProperty (Symbols.ParserTermName) == Symbols.ParserIdentifier) {
                Symbol symbol = context.Context.ModuleMap.ExpectSymbol (target);
                if (isReadonly) {
                    scope.DeclareVariableReadonly (symbol, target.SourceLocation);
                } else {
                    scope.DeclareVariableTentative (symbol, target.SourceLocation);
                }
            } else {
                SchemaDeclarer.Declare (true, isReadonly, new ExpressionContext (scope, context.Context.ModuleMap, true), target);
            }
        }
    }
}