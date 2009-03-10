using System.Collections.Generic;
using Tycho.Runtime;

namespace Tycho.Compiler {
    class CollectionSchemaContext {
        public SchemaContext SchemaContext { get; private set; }
        public List<AnyObject> Items { get; private set; }

        public CollectionSchemaContext (List<AnyObject> items, SchemaContext schemaContext) {
            SchemaContext = schemaContext;
            Items = items;
        }
    }
}