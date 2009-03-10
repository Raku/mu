using Tycho.Runtime;

namespace Tycho.Compiler {
    class PropertyAssignment : ComplexAssignment {
        AnyObject Name;
        AnyObject Expression;
        TermTransform<ExpressionContext> ExpressionTransform;
        ExpressionContext Context;

        public PropertyAssignment (AnyObject name, AnyObject expression, TermTransform<ExpressionContext> expressionTransform, ExpressionContext context, AnyObject term, AnyObject tempVariable, int frameIndex)
            : base (term, tempVariable, frameIndex) {
            Name = name;
            Expression = expression;
            ExpressionTransform = expressionTransform;
            Context = context;
        }

        public override AnyObject CompileAssignment () {
            return ByteCodeCompiler.MethodCall (
                Term,
                ExpressionTransform.Transform (Context, Expression),
                ByteCodeCompiler.LoadConstant (Term, Symbols.RuntimeSetProperty),
                ByteCodeCompiler.LoadConstant (Term, Context.ModuleMap.ExpectSymbol (Name)),
                ByteCodeCompiler.Load (Term, ByteCodeCompiler.LoadConstant (Term, TempVariable), FrameIndex));
        }
    }
}