using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    using OperatorMethodAttribute = global::System.Extensions.OperatorMethodAttribute;
    using OperatorClass = global::System.Extensions.OperatorMethodAttribute.OperatorClass;

    /// <summary>
    /// Represents operator types
    /// </summary>
    [Serializable]
    public enum OperatorKind: int
    {
        /// <summary>
        /// Unknown operator
        /// </summary>
        [Obsolete]
        Unknown = 0,

        /// <summary>
        /// Unary plus operator
        /// </summary>
        [OperatorMethod("op_UnaryPlus", "+", TypeOfOperator = OperatorClass.Unary)]
        UnaryPlus,

        /// <summary>
        /// Unary minus operator
        /// </summary>
        [OperatorMethod("op_UnaryNegation", "-", TypeOfOperator = OperatorClass.Unary)]
        UnaryNegation,

        [OperatorMethod("op_Multiply", "*", TypeOfOperator = OperatorClass.Binary)]
        Multiply,

        /// <summary>
        /// Increment
        /// </summary>
        [OperatorMethod("op_Increment", "++", TypeOfOperator = OperatorClass.Unary)]
        Increment,

        /// <summary>
        /// Decrement
        /// </summary>
        [OperatorMethod("op_Decrement", "--", TypeOfOperator = OperatorClass.Unary)]
        Decrement,

        [OperatorMethod("op_Subtraction", "-", TypeOfOperator = OperatorClass.Binary)]
        Subtraction,

        [OperatorMethod("op_Division", "/", TypeOfOperator = OperatorClass.Binary)]
        Division,

        [OperatorMethod("op_Addition", "+", TypeOfOperator = OperatorClass.Binary)]
        Addition,

        [OperatorMethod("op_BitwiseOr", "|", TypeOfOperator = OperatorClass.Binary)]
        BitwiseOr,

        [OperatorMethod("op_BitwiseAnd", "&", TypeOfOperator = OperatorClass.Binary)]
        BitwiseAnd,

        [OperatorMethod("op_Inequality", "!=", TypeOfOperator = OperatorClass.Binary)]
        Inequality,

        [OperatorMethod("op_Equality", "==", TypeOfOperator = OperatorClass.Binary)]
        Equality,

        [OperatorMethod("op_OnesComplement", "~", TypeOfOperator = OperatorClass.Unary)]
        OnesComplement,

        [OperatorMethod("op_LogicalNot", "!", TypeOfOperator = OperatorClass.Unary)]
        LogicalNot,

        [OperatorMethod("op_False", "false", TypeOfOperator = OperatorClass.Unary)]
        False,

        [OperatorMethod("op_True", "true", TypeOfOperator = OperatorClass.Unary)]
        True,

        [OperatorMethod("op_GreaterThat", ">", TypeOfOperator = OperatorClass.Binary)]
        GreaterThan,

        [OperatorMethod("op_LessThan", "<", TypeOfOperator = OperatorClass.Binary)]
        LessThat,

        [OperatorMethod("op_GreaterThatOrEqual", ">=", TypeOfOperator = OperatorClass.Binary)]
        GreaterThanOrEqual,

        [OperatorMethod("op_LessThatOrEqual", "<=", TypeOfOperator = OperatorClass.Binary)]
        LessThanOrEqual,

        [OperatorMethod("op_Explicit", "()", TypeOfOperator = OperatorClass.Binary)]
        Explicit,

        [OperatorMethod("op_Explicit", "=", TypeOfOperator = OperatorClass.Binary)]
        Implicit,
    }
}
