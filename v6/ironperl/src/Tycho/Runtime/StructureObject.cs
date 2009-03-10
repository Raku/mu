using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class StructureObject : InstanceObject {
        public IDictionary<AnyObject, AnyObject> Fields { get; private set; }

        public StructureObject (AnyObject prototype)
            : base (prototype) {
            Fields = new Dictionary<AnyObject, AnyObject> ();
        }

        public StructureObject (AnyObject prototype, params AnyObject [] items) : this (prototype) {
            Debug.Assert (items.Length % 2 == 0);

            for (int n = 0; n < items.Length; n += 2) {
                SetProperty (items [n], items [n + 1]);
            }
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeGetProperty) {
                AnyObject propertyName = arguments [0];
                return GetProperty (self, propertyName);
            } else if (name == Symbols.RuntimeSetProperty) {
                SetProperty (self, arguments [0], arguments [1]);
                return arguments [1];
            } else if (name == Symbols.RuntimeHasProperty) {
                return HasProperty (self, arguments [0]);
            } else {
                return base.InvokeMethod (self, name, arguments);
            }
        }

        public override AnyObject ActuallySerialize () {
            AnyObject str = SerializationModule.CreateStructure ();

            foreach (KeyValuePair<AnyObject, AnyObject> field in Fields) {
                str [field.Key.Expect<Symbol> ().Serialize ()] = field.Value.Serialize ();
            }

            return str;
        }

        public override AnyObject ShallowCopy () {
            StructureObject str = new StructureObject (Prototype);
            foreach (KeyValuePair<AnyObject, AnyObject> field in Fields) {
                str.Fields.Add (field.Key, field.Value);
            }

            return str;
        }

        public override bool HasProperty (AnyObject self, AnyObject name) {
            if (Fields.ContainsKey (name)) {
                return true;
            } else if (!Prototype.IsNull) {
                return Prototype.HasProperty (self, name);
            } else {
                return false;
            }
        }

        public override bool HasMethod (AnyObject self, AnyObject name) {
            return false;
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> refs = new HashSet<AnyObject> (Fields.Keys);
                refs.UnionWith (Fields.Values);

                refs.Add (Prototype);

                return refs;
            }
        }

        public override AnyObject GetProperty (AnyObject self, AnyObject name) {
            AnyObject v;
            if (Fields.TryGetValue (name, out v)) {
                return v;
            } else {
                return base.InvokeMethod (self, Symbols.RuntimeGetProperty, name);
            }
        }

        public override void SetProperty (AnyObject self, AnyObject name, AnyObject value) {
            Fields [name] = value;
        }

        public override AnyObject this [AnyObject index] {
            get {
                return GetProperty (this, index);
            }
            set {
                SetProperty (this, index, value);
            }
        }

        public override int Count {
            get {
                return Fields.Count;
            }
        }

        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            if (arguments.Length != 1) {
                return false;
            }

            AnyObject otherStruct = arguments [0];

            foreach (KeyValuePair<AnyObject, AnyObject> value in Fields) {
                if (!(otherStruct.HasProperty (value.Key) && (value.Value == null || value.Value.IsNull || value.Value.Match (results, otherStruct.GetProperty (value.Key))))) {
                    return false;
                }
            }

            return true;
        }

        public override string ToString (HashSet<AnyObject> done) {
            if (ToStringNoRecurse (done)) {
                string result = "struct {";
                bool first = true;

                foreach (KeyValuePair<AnyObject, AnyObject> value in Fields) {
                    if (value.Key != Symbols.ParserSourceLocation) {
                        if (!first) {
                            result += ", ";
                        }

                        if (value.Value.IsNull) {
                            result += value.Key.ToString (done);
                        } else {
                            result += value.Key.ToString (done) + " = " + value.Value.ToString (done);
                        }

                        first = false;
                    }
                }

                return result + "}";
            } else {
                return "...";
            }
        }

        [TychoMethodSchema ("structure", "match")]
        [TychoMethodSchema ("structure", "count")]
        [TychoMethodSchema ("structure", "properties")]
        [TychoMethodSchema ("structure", "methods")]
        [TychoMethodSchema ("structure", "object-references")]
        static AnyObject AnySchema = new AnySchemaObject ();

        [TychoMethod ("structure", "match")]
        static AnyObject PrototypeMatch (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.CreateBoolean (self.Match (arguments [0], arguments [1]));
        }

        [TychoGetter2 ("structure", "properties")]
        public static AnyObject GetProperties (StructureObject self) {
            AnyObject props = RuntimeModule.CreateSet (self.Fields.Keys);

            foreach (AnyObject item in RuntimeModule.Object.GetProperty (RuntimeModule.Structure, Symbols.RuntimeProperties)) {
                props.Add (item);
            }

            return props;
        }

        [TychoGetter2 ("structure", "methods")]
        public static AnyObject GetMethods (InstanceObject self) {
            return RuntimeModule.Object.GetProperty (self.Prototype, Symbols.RuntimeMethods);
        }

        [TychoGetter2 ("structure", "object-references")]
        public static AnyObject PrototypeGetObjectReferences (AnyObject self) {
            return RuntimeModule.CreateSet (self.ObjectReferences);
        }

        [TychoMethodSchema ("structure", "has-property")]
        static AnyObject PrototypeHasPropertySchema = new ParametersSchemaObject (new AnySchemaObject (), new AnySchemaObject ());
        [TychoMethod ("structure", "has-property")]
        static AnyObject PrototypeHasProperty (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.CreateBoolean (self.HasProperty (arguments [0]));
        }

        [TychoMethodSchema ("structure", "new")]
        static AnyObject CreateNewSchema = new UnlimitedParametersSchemaObject (1);
        [TychoMethod ("structure", "new")]
        static AnyObject CreateNew (AnyObject self, params AnyObject [] arguments) {
            if (arguments.Length % 2 != 0) throw new TychoException ("expected name/value pairs");

            return RuntimeModule.CreateStructure (arguments);
        }

        [TychoGetter ("structure", "count")]
        static AnyObject GetSize (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.CreateInteger (self.Count);
        }
    }
}
