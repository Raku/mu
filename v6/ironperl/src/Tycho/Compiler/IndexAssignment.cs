using Tycho.Runtime;

namespace Tycho.Compiler {
    class IndexAssignment : ComplexAssignment {
        AnyObject Index;
        AnyObject Expression;
        TermTransform<ExpressionContext> ExpressionTransform;
        ExpressionContext Context;

        public IndexAssignment (AnyObject index, AnyObject expression, TermTransform<ExpressionContext> expressionTransform, ExpressionContext context, AnyObject term, AnyObject tempVariable, int frameIndex)
            : base (term, tempVariable, frameIndex) {
            Index = index;
            Expression = expression;
            ExpressionTransform = expressionTransform;
            Context = context;
        }

        public override AnyObject CompileAssignment () {
            return ByteCodeCompiler.MethodCall (
                Term,
                ExpressionTransform.Transform (Context, Expression),
                ByteCodeCompiler.LoadConstant (Term, Symbols.RuntimeIndexSet),
                ExpressionTransform.Transform (Context, Index),
                ByteCodeCompiler.Load (Term, ByteCodeCompiler.LoadConstant (Term, TempVariable), FrameIndex));
        }
    }
}