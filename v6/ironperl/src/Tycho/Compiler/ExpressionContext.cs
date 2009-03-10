namespace Tycho.Compiler {
    class ExpressionContext {
        public FrameSymbolScope Scope { get; private set; }
        public ModuleMap ModuleMap { get; private set; }
        public bool AtModuleLevel { get; private set; }

        public ExpressionContext (FrameSymbolScope scope, ModuleMap moduleMap, bool atModuleLevel) {
            Scope = scope;
            ModuleMap = moduleMap;
            AtModuleLevel = atModuleLevel;
        }

        public ExpressionContext CreateNestedScope () {
            return new ExpressionContext (new FrameSymbolScope (Scope), new ModuleMap (ModuleMap), false);
        }
    }
}