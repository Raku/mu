using System;
using System.Collections.Generic;
using System.IO;
using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Compiler {
    public class FrameSymbolScope : SymbolScope {
        public class Variable {
            public bool IsReadonly;

            public Variable (bool isReadonly) {
                IsReadonly = isReadonly;
            }
        }

        SymbolScope Parent;
        Dictionary<AnyObject, Variable> Symbols;

        public FrameSymbolScope (SymbolScope parent) {
            Parent = parent;
            Symbols = new Dictionary<AnyObject, Variable> ();
        }

        public override int FindFrameIndex (AnyObject name, bool forWriting, int index, SourceLocation sloc) {
            Variable variable;
            if (Symbols.TryGetValue (name, out variable)) {
                if (variable.IsReadonly && forWriting) {
                    throw new VariableReadonlyException (name, sloc);
                } else {
                    return index;
                }
            } else if (Parent != null) {
                return Parent.FindFrameIndex (name, forWriting, index + 1, sloc);
            } else {
                return SymbolNotFound;
            }
        }

        public override string PrettyPrint () {
            var output = new StringWriter ();

            if (Parent != null) {
                output.Write(Parent.PrettyPrint ());
                output.WriteLine ();
            }

            foreach (var sym in Symbols) {
                output.WriteLine (sym + ", " + sym.GetHashCode ());
            }

            return output.ToString ();
        }

        public int DeclareVariableTentative (AnyObject name, SourceLocation sloc) {
            int frameIndex = FindFrameIndex (name, true, 0, sloc);

            if (frameIndex == SymbolNotFound) {
                Symbols.Add (name, new Variable (false));
                return 0;
            } else {
                // variable already declared, return it's existing frame index
                return frameIndex;
            }
        }

        public int DeclareVariable (AnyObject name, SourceLocation sloc) {
            return DeclareVariable (name, sloc, false);
        }

        public int DeclareVariable (AnyObject name, SourceLocation sloc, bool isReadonly) {
            int frameIndex = FindFrameIndex (name, false, 0, sloc);

            if (frameIndex == SymbolNotFound) {
                Symbols.Add (name, new Variable (isReadonly));
                return 0;
            } else {
                throw new VariableAlreadyDeclaredException(name, sloc);
            }
        }

        public int DeclareVariableReadonly (AnyObject name, SourceLocation sloc) {
            return DeclareVariable (name, sloc, true);
        }

        public int DeclareVariableOverride (AnyObject name, SourceLocation sloc) {
            int frameIndex = FindFrameIndex (name, false, 0, sloc);

            // we redeclare the variable if its in an outer frame but not the current one (the zeroeth one).
            if (frameIndex != 0) {
                Symbols.Add (name, new Variable (false));
                return 0;
            } else {
                throw new VariableAlreadyDeclaredException (name, sloc);
            }
        }
    }
}