using System.Collections.Generic;
using System.IO;
using Tycho.Language;
using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Compiler {
    public class ModuleSymbolScope : SymbolScope {
        Dictionary<AnyObject, SymbolScope> Modules;
        private IModuleScopeLoader ModuleLoader;

        public ModuleSymbolScope (IModuleScopeLoader moduleLoader) {
            Modules = new Dictionary<AnyObject, SymbolScope> ();
            ModuleLoader = moduleLoader;
        }

        public override int FindFrameIndex (AnyObject name, bool forWriting, int index, SourceLocation sloc) {
            Symbol symbol = name as Symbol;

            if (symbol == null) {
                return SymbolNotFound;
            }

            SymbolScope symbols;

            if (GetModule (symbol.Namespace, out symbols)) {
                var frameIndex = symbols.FindFrameIndex (name, forWriting, index, sloc);
                if (frameIndex == SymbolNotFound) {
                    return frameIndex;
                } else if (forWriting) {
                    throw new VariableReadonlyException (name, sloc);
                } else {
                    return frameIndex;
                }
            } else {
                return SymbolNotFound;
            }
        }

        public override string PrettyPrint () {
            var output = new StringWriter ();

            foreach (SymbolScope module in Modules.Values) {
                output.Write (module.PrettyPrint ());
            }

            return output.ToString ();
        }

        private bool GetModule (Namespace ns, out SymbolScope symbols) {
            if (Modules.TryGetValue (ns, out symbols)) {
                return true;
            } else {
                symbols = ModuleLoader.LoadModule (ns);

                if (symbols != null) {
                    Modules.Add (ns, symbols);
                    return true;
                } else {
                    return false;
                }
            }
        }

        public void DeclareModule (AnyObject module) {
            AnyObject ns = module.GetProperty (Symbols.RuntimeNamespace);

            Modules [ns] = ExpressionLanguage.BuildScopeRecursive (module);
        }
    }
}