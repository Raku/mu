using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Parser.Tokens;
using Tycho.Parser;

namespace Tycho.Compiler {
    public class CompilationException : TychoException {
        public SourceLocation SourceLocation { get; private set; }

        public CompilationException (string message, AnyObject term)
            : base (message) {
            SourceLocation = term.SourceLocation;
        }

        public CompilationException (string message, SourceLocation sloc)
            : base (message) {
            SourceLocation = sloc;
        }

        public static CompilationException TermNotRecognised (AnyObject term) {
            return new TermNotRecognisedException (term);
        }
    }

    public class TermNotRecognisedException : CompilationException {
        public TermNotRecognisedException (AnyObject term) : base ("term not recognised: " + term, term) {
        }
    }

    public class VariableNotDeclaredException : CompilationException {
        public VariableNotDeclaredException (AnyObject variable, SourceLocation sloc)
            : base ("variable " + variable + " not declared", sloc) { }
    }

    public class VariableAlreadyDeclaredException : CompilationException {
        public VariableAlreadyDeclaredException (AnyObject variable, SourceLocation sloc)
            : base ("variable `" + variable + "' already declared", sloc) { }
    }

    public class VariableReadonlyException : CompilationException {
        public VariableReadonlyException (AnyObject variable, SourceLocation sloc)
            : base ("variable `" + variable + "' cannot be written to", sloc) { }
    }
}
