using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class TychoException : Exception {
        public TychoException (string message) : base (message) { }
        public TychoException (string message, Exception innerException) : base (message, innerException) { }

        public static TychoException NoSuchMethod (AnyObject name) {
            return new TychoException ("no such method " + name);
        }

        public static TychoException NoSuchProperty (AnyObject name) {
            return new NoSuchPropertyException (name);
        }

        public static TychoException InvalidArguments () {
            return new TychoException ("invalid arguments");
        }

        public static TychoException NoMatchingMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            return new NoMatchingMethodException (self, name, arguments);
        }

        public static TychoException NoSuchVariable (AnyObject name) {
            return new TychoException ("variable " + name + " not found");
        }

        public static TychoException ReadOnlyProperty (AnyObject name) {
            return new TychoException ("property " + name + " is readonly");
        }

        public static TychoException AssignmentMatchFailed (AnyObject pattern, AnyObject result) {
            return new TychoException ("right hand side of assignment " + result + " failed to match left hand side " + pattern);
        }

        public static TychoException MemberAlreadyDefined (AnyObject memberName) {
            return new MemberAlreadyDefinedException (memberName);
        }
    }

    public class ExitException : TychoException {
        public ExitException () : base ("exit") { }
    }

    public class NoSuchPropertyException : TychoException {
        public NoSuchPropertyException (AnyObject name)
            : base ("no such property " + name) {
        }
    }

    public class NoMatchingMethodException : TychoException {
        public NoMatchingMethodException (AnyObject self, AnyObject name, AnyObject [] arguments)
            : base (ConstructMessage (self, name, arguments)) {
        }

        private static string ConstructMessage (AnyObject self, AnyObject name, AnyObject [] arguments) {
            string argumentsString = "(" + String.Join (", ", arguments.Select (a => a != null ? a.ToString () : "null").ToArray ()) + ")";
            return "no such method " + name + " for " + self + " that can match the arguments " + argumentsString;
        }
    }

    public class MemberAlreadyDefinedException : TychoException {
        public MemberAlreadyDefinedException (AnyObject memberName) : base (String.Format ("member `{0}' already defined", memberName)) {
        }
    }
}
