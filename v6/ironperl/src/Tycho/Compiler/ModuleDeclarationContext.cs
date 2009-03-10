namespace Tycho.Compiler {
    class ModuleDeclarationContext {
        public ExpressionContext Context { get; private set; }
        public FrameSymbolScope ModuleScope { get; private set; }

        public ModuleDeclarationContext (ExpressionContext context, FrameSymbolScope moduleScope) {
            Context = context;
            ModuleScope = moduleScope;
        }
    }
}