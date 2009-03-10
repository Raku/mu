using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Compiler {
    public class Term : StructureObject {
        public Term (SourceLocation sourceLocation, params AnyObject [] fields) : base (RuntimeModule.Null, fields) {
            InternalSourceLocation = sourceLocation;
        }

        private SourceLocation InternalSourceLocation;
        public override SourceLocation SourceLocation {
            get { return InternalSourceLocation; }
            set { InternalSourceLocation = value; }
        }

        public override bool HasSourceLocation {
            get {
                return true;
            }
        }
    }
}