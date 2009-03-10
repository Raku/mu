using System;
using System.Collections.Generic;
using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Compiler {
    public class CompilerModule {
        protected static IPrimitiveFactoryModule Current = new DefaultPrimitiveFactoryModule ();

        public static AnyObject CreateTerm (SourceLocation sourceLocation, params AnyObject [] fields) {
            return new Term (sourceLocation, fields);
        }

        public static AnyObject CreateStructure (params AnyObject [] fields) {
            return new StructureObject (RuntimeModule.Null, fields);
        }

        public static AnyObject CreateTermList (IEnumerable<AnyObject> items) {
            return new ListObject (RuntimeModule.Null, items);
        }

        public static AnyObject CreateTermList () {
            return new ListObject (RuntimeModule.Null);
        }

        public static AnyObject CreateList (IEnumerable<AnyObject> items) {
            return new ListObject (RuntimeModule.Null, items);
        }

        public static AnyObject CreateInteger (int n) {
            return new IntegerObject (RuntimeModule.Null, n);
        }

        public static AnyObject CreateReal (double n) {
            return new RealObject (RuntimeModule.Null, n);
        }

        public static AnyObject CreateBoolean (bool b) {
            return new BooleanObject (RuntimeModule.Null, b);
        }

        public static AnyObject CreateString (string s) {
            return new StringObject (RuntimeModule.Null, s);
        }

        public static AnyObject CreateDateTime (DateTime d) {
            return new DateTimeObject (RuntimeModule.Null, d);
        }
    }
}