using System.Collections.Generic;
using Tycho.Runtime;

namespace Tycho.Compiler {
    class CollectionExpressionContext {
        public List<AnyObject> Items { get; private set; }
        public ExpressionContext Context { get; private set; }

        public CollectionExpressionContext (ExpressionContext context, List<AnyObject> items) {
            Context = context;
            Items = items;
        }
    }
}