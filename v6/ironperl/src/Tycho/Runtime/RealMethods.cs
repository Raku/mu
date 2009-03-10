using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public static class RealMethods {
        [TychoMethod2 ("real", "plus")]
        public static double Plus (double self, double arg) {
            return self + arg;
        }

        [TychoMethod2 ("real", "plus")]
        public static double PlusInteger (double self, int arg) {
            return self + arg;
        }

        [TychoMethod2 ("real", "plus", SchemaName = "plus-string")]
        public static string PlusString (double self, string arg) {
            return self + arg;
        }

        [TychoMethod2 ("real", "minus")]
        public static double Minus (double self, double arg) {
            return self - arg;
        }

        [TychoMethod2 ("real", "minus", SchemaName = "minus-int")]
        public static double MinusInteger (double self, int arg) {
            return self - arg;
        }

        [TychoMethod2 ("real", "multiply")]
        public static double Multiply (double self, double arg) {
            return self * arg;
        }

        [TychoMethod2 ("real", "multiply", SchemaName = "multiply-int")]
        public static double MultiplyInteger (double self, int arg) {
            return self * arg;
        }

        [TychoMethod2 ("real", "divide")]
        public static double Divide (double self, double arg) {
            return self / arg;
        }

        [TychoMethod2 ("real", "divide", SchemaName = "divide-int")]
        public static double DivideInteger (double self, int arg) {
            return self / arg;
        }

        [TychoMethod2 ("real", "to-power")]
        public static double ToPower (double self, double arg) {
            return Math.Pow (self, arg);
        }

        [TychoMethod2 ("real", "to-power", SchemaName = "to-power-int")]
        public static double ToPowerInteger (double self, int arg) {
            return Math.Pow (self, arg);
        }

        [TychoMethod2 ("real", "less-than")]
        public static bool LessThan (double self, double arg) {
            return self < arg;
        }

        [TychoMethod2 ("real", "less-than")]
        public static bool LessThan (double self, int arg) {
            return self < arg;
        }

        [TychoMethod2 ("real", "less-than-equal-to")]
        public static bool LessThanEqualTo (double self, double arg) {
            return self <= arg;
        }

        [TychoMethod2 ("real", "greater-than")]
        public static bool GreaterThan (double self, double arg) {
            return self > arg;
        }

        [TychoMethod2 ("real", "greater-than")]
        public static bool GreaterThan (double self, int arg) {
            return self > arg;
        }

        [TychoMethod2 ("real", "greater-than-equal-to")]
        public static bool GreaterThanEqualTo (double self, double arg) {
            return self >= arg;
        }

        [TychoMethod2 ("real", "negative")]
        public static double Negative (double self) {
            return -self;
        }
    }
}
