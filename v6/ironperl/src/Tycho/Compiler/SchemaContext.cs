using System.Collections.Generic;

namespace Tycho.Compiler {
    class SchemaContext {
        public bool Assignment { get; private set; }
        /// <summary>
        /// Normal scope is for looking up variables used to create the schema
        /// 
        /// The reason for the two scopes is that in the case of closures
        /// the schemas are created in the stack frame creating the closure
        /// while the variables in the parameter list are in the stack frame
        /// inside the closure.
        /// </summary>
        public ExpressionContext Context { get; private set; }
        public List<ComplexAssignment> ComplexAssignments { get; private set; }

        public SymbolScope ConstructionScope { get; private set; }
        public bool ForDeclaration { get; private set; }

        public SchemaContext (bool assignment, ExpressionContext context, SymbolScope constructionScope, bool forDeclaration) {
            Assignment = assignment;
            Context = context;
            ComplexAssignments = new List<ComplexAssignment> ();
            ForDeclaration = forDeclaration;
            ConstructionScope = constructionScope;
        }
    }
}