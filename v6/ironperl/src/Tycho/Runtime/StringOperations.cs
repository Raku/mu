using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public static class StringOperations {
        [TychoMethod2 ("string", "plus")]
        public static AnyObject Plus (string self, AnyObject argument) {
            return RuntimeModule.CreateString (self + argument);
        }

        [TychoMethod2 ("string", "multiply")]
        public static AnyObject Multiply (string self, int repeat) {
            return RuntimeModule.CreateString (RepeatString (self, repeat));
        }

        [TychoGetter2 ("string", "count")]
        public static int Count (string self) {
            return self.Length;
        }

        public static string RepeatString (string s, int repeat) {
            return new StringBuilder (s.Length * repeat).Insert (0, s, repeat).ToString ();
        }
    }
}
