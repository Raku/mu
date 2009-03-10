using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public static class StringMethods {
        [TychoMethod2 ("string", "plus")]
        public static string Plus (string self, AnyObject arg) {
            StringObject str;
            if (arg.TryCastTo (out str)) {
                return self + str.Value;
            } else {
                return self + arg;
            }
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
