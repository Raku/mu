using Tycho.Runtime;

namespace Tycho.Compiler {
    abstract class ComplexAssignment {
        public AnyObject Term { get; private set; }
        public AnyObject TempVariable { get; private set; }
        public int FrameIndex { get; private set; }

        public ComplexAssignment (AnyObject term, AnyObject tempVariable, int frameIndex) {
            Term = term;
            TempVariable = tempVariable;
            FrameIndex = frameIndex;
        }

        public abstract AnyObject CompileAssignment ();
    }
}