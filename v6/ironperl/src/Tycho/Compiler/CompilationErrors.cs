using System.Collections.Generic;
using Tycho.Runtime;

namespace Tycho.Compiler {
    class CompilationErrors : List<CompilationException> {
        public AnyObject CreateErrorTerm (string message, AnyObject term) {
            return CreateErrorTerm (new CompilationException (message, term));
        }

        public AnyObject CreateErrorTerm (CompilationException cex) {
            Add (cex);

            AnyObject errorTerm = CompilerModule.CreateTerm (cex.SourceLocation,
                                                             Symbols.ParserTermName, Symbols.ParserError,
                                                             Symbols.ParserError, cex.Message);

            return errorTerm;
        }
    }
}