using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class ValueObject<T> : InstanceObject {
        public T Value { get; private set; }

        public ValueObject (AnyObject prototype, T v) : base (prototype) {
            Value = v;
        }

        public override int GetHashCode () {
            return Value.GetHashCode ();
        }

        public override bool Equals (object obj) {
            if (obj is AnyObject) {
                AnyObject anyObject = obj as AnyObject;
                ValueObject<T> valueObj;
                if (anyObject.TryCastTo (out valueObj)) {
                    return Value.Equals (valueObj.Value);
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }

        public override string ToString (HashSet<AnyObject> done) {
            if (Value is string) {
                return "\"" + Value + "\"";
            } else if (Value is Boolean) {
                return Value.ToString ().ToLower ();
            } else {
                return Value.ToString ();
            }
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                return new List<AnyObject> ();
            }
        }

        public static AnyObject MatchValue (AnyObject self, params AnyObject [] arguments) {
            if (self is ValueObject<T>) {
                return RuntimeModule.CreateBoolean (arguments [1] is ValueObject<T> && self == arguments [1]);
            } else {
                return RuntimeModule.CreateBoolean (arguments [1] is ValueObject<T>);
            }
        }

        public override bool Match (AnyObject results, params AnyObject [] obj) {
            return Equals (obj [0]);
        }

        public override int CompareTo (AnyObject other) {
            return ((IComparable<T>) Value).CompareTo (other.ExpectValue<T> ());
        }

        public override AnyObject ShallowCopy () {
            return this;
        }

        public override AnyObject Serialize () {
            if (typeof (T) == typeof (int)
                || typeof (T) == typeof (double)
                || typeof (T) == typeof (string)
                || typeof (T) == typeof (bool)
                || typeof (T) == typeof (DateTime)) {
                return this;
            } else {
                return base.Serialize ();
            }
        }

        public override AnyObject ActuallySerialize () {
            return Serialize ();
        }
    }

    public class IntegerObject : ValueObject<int> {
        public IntegerObject (AnyObject prototype, int i) : base (prototype, i) { }

        public override AnyObject Plus (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value + (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value + (arg as RealObject).Value;
            } else if (arg is StringObject) {
                return Value + (arg as StringObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimePlus);
            }
        }

        public override AnyObject Minus (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value - (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value - (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeMinus);
            }
        }

        public override AnyObject Multiply (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value * (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value * (arg as RealObject).Value;
            } else if (arg is StringObject) {
                return StringMethods.RepeatString ((arg as StringObject).Value, Value);
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeMultiply);
            }
        }

        public override AnyObject Divide (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value / (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value / (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeDivide);
            }
        }

        public override AnyObject LessThan (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value < (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value < (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeLessThan);
            }
        }

        public override AnyObject LessThanEqualTo (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value <= (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value <= (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeLessThanEqualTo);
            }
        }

        public override AnyObject GreaterThan (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value > (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value > (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeGreaterThan);
            }
        }

        public override AnyObject GreaterThanEqualTo (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value >= (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value >= (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeGreaterThanEqualTo);
            }
        }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }
    }

    public class RealObject : ValueObject<double> {
        public RealObject (AnyObject prototype, double i) : base (prototype, i) { }

        public override AnyObject Plus (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value + (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value + (arg as RealObject).Value;
            } else if (arg is StringObject) {
                return Value + (arg as StringObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimePlus);
            }
        }

        public override AnyObject Minus (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value - (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value - (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeMinus);
            }
        }

        public override AnyObject Multiply (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value * (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value * (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeMultiply);
            }
        }

        public override AnyObject Divide (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value / (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value / (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeDivide);
            }
        }

        public override AnyObject LessThan (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value < (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value < (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeLessThan);
            }
        }

        public override AnyObject LessThanEqualTo (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value <= (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value <= (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeLessThanEqualTo);
            }
        }

        public override AnyObject GreaterThan (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value > (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value > (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeGreaterThan);
            }
        }

        public override AnyObject GreaterThanEqualTo (AnyObject arg) {
            if (arg is IntegerObject) {
                return Value >= (arg as IntegerObject).Value;
            } else if (arg is RealObject) {
                return Value >= (arg as RealObject).Value;
            } else {
                throw TychoException.NoSuchMethod (Symbols.RuntimeGreaterThanEqualTo);
            }
        }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }
    }

    public class StringObject : ValueObject<string> {
        public StringObject (AnyObject prototype, string i) : base (prototype, i) { }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }
    }

    public class BooleanObject : ValueObject<bool> {
        public BooleanObject (AnyObject prototype, bool i) : base (prototype, i) { }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }
    }

    public class DateTimeObject : ValueObject<DateTime> {
        public DateTimeObject (AnyObject prototype, DateTime i) : base (prototype, i) { }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }
    }

    public static class ValueObjectMethods {
        [TychoModuleLoad]
        static void InstallPrimitives (AnyObject module) {
            module [Symbols.RuntimeInteger] = RuntimeModule.Integer;
            RuntimeModule.Integer.AddMethod (Symbols.RuntimeMatch, new NativeMethod (new AnySchemaObject (), IntegerObject.MatchValue));

            module [Symbols.RuntimeBoolean] = RuntimeModule.Boolean;
            RuntimeModule.Boolean.AddMethod (Symbols.RuntimeMatch, new NativeMethod (new AnySchemaObject (), BooleanObject.MatchValue));

            module [Symbols.RuntimeReal] = RuntimeModule.Real;
            RuntimeModule.Real.AddMethod (Symbols.RuntimeMatch, new NativeMethod (new AnySchemaObject (), RealObject.MatchValue));

            module [Symbols.RuntimeString] = RuntimeModule.String;
            RuntimeModule.String.AddMethod (Symbols.RuntimeMatch, new NativeMethod (new AnySchemaObject (), StringObject.MatchValue));

            module [Symbols.RuntimeDateTime] = RuntimeModule.DateTime;
        }
    }
}
