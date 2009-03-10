using System.Diagnostics;
using System.Linq;
using System.Text;
using Tycho.Parser.Tokens;
using Tycho.Runtime;
using Tycho.Parser;

namespace Tycho.Compiler {
    public abstract class SymbolScope {
        protected const int SymbolNotFound = -1, SymbolReadonly = -2;

        public int GetFrameIndex (AnyObject name, bool forWriting, SourceLocation sloc) {
            int index = FindFrameIndex (name, forWriting, 0, sloc);

            if (index != SymbolNotFound) {
                return index;
            } else {
                throw new VariableNotDeclaredException (name, sloc);
            }
        }

        public abstract int FindFrameIndex (AnyObject name, bool forWriting, int index, SourceLocation sloc);

        public abstract string PrettyPrint ();
    }
}
