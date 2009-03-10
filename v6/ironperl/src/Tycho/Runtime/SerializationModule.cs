using System;
using System.Collections.Generic;

namespace Tycho.Runtime {
    public class SerializationModule {
        protected static IPrimitiveFactoryModule Current = new DefaultPrimitiveFactoryModule ();

        public static AnyObject CreateStructure (params AnyObject [] fields) {
            return Current.CreateStructure (fields);
        }

        public static AnyObject CreateList (IEnumerable<AnyObject> items) {
            return Current.CreateList (items);
        }

        public static AnyObject CreateInteger (int n) {
            return Current.CreateInteger (n);
        }

        public static AnyObject CreateReal (double n) {
            return Current.CreateReal (n);
        }

        public static AnyObject CreateBoolean (bool b) {
            return Current.CreateBoolean (b);
        }

        public static AnyObject CreateString (string s) {
            return Current.CreateString (s);
        }

        public static AnyObject CreateDateTime (DateTime d) {
            return Current.CreateDateTime (d);
        }
    }
}
