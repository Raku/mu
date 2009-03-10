using System;
using System.Collections.Generic;

namespace Tycho.Runtime {
    public class DefaultPrimitiveFactoryModule : IPrimitiveFactoryModule {
        public AnyObject CreateStructure (params AnyObject[] fields) {
            return new StructureObject (RuntimeModule.Structure, fields);
        }

        public AnyObject CreateList (IEnumerable<AnyObject> items) {
            return new ListObject (RuntimeModule.List, items);
        }

        public AnyObject CreateList (params AnyObject [] items) {
            return new ListObject (RuntimeModule.List, items);
        }

        public AnyObject CreateInteger (int n) {
            return new IntegerObject (RuntimeModule.Integer, n);
        }

        public AnyObject CreateReal (double n) {
            return new RealObject (RuntimeModule.Real, n);
        }

        public AnyObject CreateBoolean (bool b) {
            return new BooleanObject (RuntimeModule.Boolean, b);
        }

        public AnyObject CreateString (string s) {
            return new StringObject (RuntimeModule.String, s);
        }

        public AnyObject CreateDateTime (DateTime d) {
            return new DateTimeObject (RuntimeModule.DateTime, d);
        }
    }
}