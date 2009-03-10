using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public static class IntegerMethods {
        [TychoMethod2 ("integer", "plus")]
        public static int Plus (int self, int arg) {
            return self + arg;
        }

        [TychoMethod2 ("integer", "plus")]
        public static double PlusReal (int self, double arg) {
            return self + arg;
        }

        [TychoMethod2 ("integer", "plus")]
        public static string PlusString (int self, string arg) {
            return self + arg;
        }

        [TychoMethod2 ("integer", "minus")]
        public static int Minus (int self, int arg) {
            return self - arg;
        }

        [TychoMethod2 ("integer", "minus")]
        public static double MinusReal (int self, double arg) {
            return self - arg;
        }

        [TychoMethod2 ("integer", "multiply")]
        public static int Multiply (int self, int arg) {
            return self * arg;
        }

        [TychoMethod2 ("integer", "multiply")]
        public static double MultiplyReal (int self, double arg) {
            return self * arg;
        }

        [TychoMethod2 ("integer", "multiply")]
        public static string MultiplyString (int self, string arg) {
            return StringMethods.RepeatString (arg, self);
        }

        [TychoMethod2 ("integer", "divide")]
        public static int Divide (int self, int arg) {
            return self / arg;
        }

        [TychoMethod2 ("integer", "divide")]
        public static double DivideReal (int self, double arg) {
            return self / arg;
        }

        [TychoMethod2 ("integer", "to-power")]
        public static int ToPower (int self, int arg) {
            return (int) Math.Pow (self, arg);
        }

        [TychoMethod2 ("integer", "to-power")]
        public static double ToPowerReal (int self, double arg) {
            return Math.Pow (self, arg);
        }

        [TychoMethod2 ("integer", "less-than")]
        public static bool LessThan (int self, int arg) {
            return self < arg;
        }

        [TychoMethod2 ("integer", "less-than")]
        public static bool LessThan (int self, double arg) {
            return self < arg;
        }

        [TychoMethod2 ("integer", "less-than-equal-to")]
        public static bool LessThanEqualTo (int self, int arg) {
            return self <= arg;
        }

        [TychoMethod2 ("integer", "greater-than")]
        public static bool GreaterThan (int self, int arg) {
            return self > arg;
        }

        [TychoMethod2 ("integer", "greater-than")]
        public static bool GreaterThan (int self, double arg) {
            return self > arg;
        }

        [TychoMethod2 ("integer", "greater-than-equal-to")]
        public static bool GreaterThanEqualTo (int self, int arg) {
            return self >= arg;
        }

        [TychoMethod2 ("integer", "negative")]
        public static int Negative (int self) {
            return -self;
        }
    }
}
