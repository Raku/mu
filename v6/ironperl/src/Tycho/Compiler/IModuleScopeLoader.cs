using System.Collections.Generic;
using Tycho.Runtime;

namespace Tycho.Compiler {
    public interface IModuleScopeLoader {
        SymbolScope LoadModule (Namespace ns);
    }
}