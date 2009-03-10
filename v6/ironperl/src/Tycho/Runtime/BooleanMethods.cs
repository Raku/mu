using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public static class BooleanMethods {
        [TychoMethod2 ("boolean", "not")]
        public static bool Not (bool b) {
            return !b;
        }

        [TychoMethod2 ("boolean", "and")]
        public static bool And (bool self, bool other) {
            return self && other;
        }

        [TychoMethod2 ("boolean", "or")]
        public static bool Or (bool self, bool other) {
            return self || other;
        }

        [TychoMethod2 ("boolean", "xor")]
        public static bool Xor (bool self, bool other) {
            return self ^ other;
        }
    }
}
