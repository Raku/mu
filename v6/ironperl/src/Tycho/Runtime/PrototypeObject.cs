using System;

using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class PrototypeObject : AnyObject {
        AnyObject Prototype;
        IDictionary<AnyObject, Slot> Slots = new Dictionary<AnyObject, Slot> ();
        private string Name;

        public PrototypeObject (AnyObject prototype) : this (prototype, null) {
        }

        public PrototypeObject (AnyObject prototype, string name) {
            Prototype = prototype;
            Debug.Assert (Prototype != null);
            Name = name;
        }

        public override AnyObject GetProperty (AnyObject self, AnyObject name) {
            Slot slot;

            if (Slots.TryGetValue (name, out slot)) {
                if (slot is FieldSlot) {
                    return ((FieldSlot) slot).Value;
                } else if (slot is PropertySlot) {
                    return ((PropertySlot) slot).GetValue (self);
                } else {
                    throw TychoException.NoSuchProperty (name);
                }
            } else if (!Prototype.IsNull) {
                return Prototype.GetProperty (self, name);
            } else {
                throw TychoException.NoSuchProperty (name);
            }
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> items = new HashSet<AnyObject> ();
                items.UnionWith (Slots.Keys);

                foreach (Slot slot in Slots.Values) {
                    if (slot is FieldSlot) {
                        items.Add (((FieldSlot) slot).Value);
                    } else if (slot is PropertySlot) {
                        PropertySlot pslot = (PropertySlot) slot;
                        if (pslot.Getter != null) {
                            items.Add (pslot.Getter);
                        }
                        if (pslot.Setter != null) {
                            items.Add (pslot.Setter);
                        }
                    } else if (slot is MethodSlot) {
                        MethodSlot mslot = (MethodSlot) slot;

                        foreach (Operation method in mslot.Methods) {
                            items.Add (method);
                        }
                    } else {
                        Debug.Fail ("didn't expect slot to be of type " + slot.GetType ());
                    }
                }

                return items;
            }
        }

        public override void SetProperty (AnyObject self, AnyObject name, AnyObject value) {
            if (name == Symbols.RuntimeProperties) {
                throw TychoException.ReadOnlyProperty (name);
            } else if (name == Symbols.RuntimeMethods) {
                throw TychoException.ReadOnlyProperty (name);
            } else {
                Slot slot = FindSlot (name);

                if (slot == null) {
                    Slots [name] = new FieldSlot (value);
                } else if (slot is FieldSlot) {
                    Slots [name] = new FieldSlot (value);
                } else if (slot is PropertySlot) {
                    ((PropertySlot) slot).SetValue (self, value);
                } else {
                    throw TychoException.NoSuchProperty (name);
                }
            }
        }

        public override bool HasProperty (AnyObject self, AnyObject name) {
            if (name == Symbols.RuntimeProperties || name == Symbols.RuntimeMethods) {
                return true;
            } else {
                Slot slot = FindSlot (name);

                if (slot is FieldSlot || slot is PropertySlot) {
                    return true;
                } else if (!Prototype.IsNull) {
                    return Prototype.HasProperty (self, name);
                } else {
                    return false;
                }
            }
        }

        public override bool HasMethod (AnyObject self, AnyObject name) {
            Slot slot = FindSlot (name);

            if (slot is MethodSlot) {
                return true;
            } else if (!Prototype.IsNull) {
                return Prototype.HasMethod (self, name);
            } else {
                return false;
            }
        }

        Slot FindSlot (AnyObject name) {
            Slot slot;
            PrototypeObject prototype;

            if (Slots.TryGetValue (name, out slot)) {
                return slot;
            } else if (!Prototype.IsNull && Prototype.TryCastTo (out prototype)) {
                return prototype.FindSlot (name);
            } else {
                return null;
            }
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeGetProperty) {
                return GetProperty (self, arguments [0]);
            } else if (name == Symbols.RuntimeSetProperty) {
                SetProperty (self, arguments [0], arguments [1]);
                return arguments [1];
            } else if (name == Symbols.RuntimeHasProperty) {
                return HasProperty (self, arguments [0]);
            } else if (name == Symbols.RuntimeHasMethod) {
                return HasMethod (self, arguments [0]);
            } else {
                Slot slot;

                if (Slots.TryGetValue (name, out slot) && slot is MethodSlot) {
                    MethodSlot methodSlot = (MethodSlot) slot;
                    AnyObject [] selfArgs = new AnyObject [arguments.Length + 1];
                    arguments.CopyTo (selfArgs, 1);
                    selfArgs [0] = self;

                    AnyObject result;
                    if (methodSlot.TryInvoke (out result, selfArgs)) {
                        return result;
                    }
                }
                
                if (!Prototype.IsNull) {
                    return Prototype.InvokeMethod (self, name, arguments);
                } else {
                    throw TychoException.NoMatchingMethod (self, name, arguments);
                }
            }
        }

        public override AnyObject ShallowCopy () {
            var prot = new PrototypeObject (Prototype, Name);

            foreach (var slot in Slots) {
                prot.Slots.Add (slot.Key, slot.Value);
            }

            return prot;
        }

        public override AnyObject ActuallySerialize () {
            return SerializationModule.CreateStructure (Symbols.RuntimePrototype, RuntimeModule.Null);
        }

        public override void AddMethod (AnyObject name, AnyObject method) {
            Slot slot;

            if (Slots.TryGetValue (name, out slot) && slot is MethodSlot) {
                ((MethodSlot) slot).Methods.Add (method);
            } else {
                MethodSlot methodSlot = new MethodSlot ();
                methodSlot.Methods.Add (method);
                Slots [name] = methodSlot;
            }
        }

        public override void AddPropertyGetter (AnyObject name, AnyObject getter, bool overrideMember) {
            AddPropertyGetterSetter (name, prop => prop.Getter != null, prop => prop.Getter = getter, overrideMember);
        }

        private void AddPropertyGetterSetter (AnyObject name, Predicate<PropertySlot> isAlreadyDefined, Action<PropertySlot> define, bool overrideMember) {
            Slot slot;
            PropertySlot prop;

            if (Slots.TryGetValue (name, out slot)) {
                if (slot is PropertySlot) {
                    prop = slot as PropertySlot;
                    if (!isAlreadyDefined (prop) || overrideMember) {
                        define (prop);
                        return;
                    }
                }

                if (!overrideMember) {
                    throw TychoException.MemberAlreadyDefined (name);
                }
            }

            prop = new PropertySlot ();
            define (prop);
            Slots [name] = prop;
        }

        public override void AddPropertySetter (AnyObject name, AnyObject setter, bool overrideMember) {
            AddPropertyGetterSetter (name, prop => prop.Setter != null, prop => prop.Setter = setter, overrideMember);
        }

        public override void AddField (AnyObject name, AnyObject value, bool overrideField) {
            Slot slot;

            if (Slots.TryGetValue (name, out slot)) {
                if ((bool) overrideField) {
                    Slots [name] = new FieldSlot (value);
                } else {
                    throw TychoException.MemberAlreadyDefined (name);
                }
            } else {
                if ((bool) overrideField) {
                    throw new TychoException (String.Format ("field `{0}' does not exist to be overriden", name));
                } else {
                    Slots [name] = new FieldSlot (value);
                }
            }
        }

        public override string ToString (HashSet<AnyObject> done) {
            return (Name != null? Name + " ": Name) + "prototype";
        }

        [TychoMethodSchema ("object", "methods")]
        [TychoMethodSchema ("object", "object-references")]
        [TychoMethodSchema ("object", "properties")]
        [TychoMethodSchema ("object", "match")]
        static AnyObject AnySchema = new AnySchemaObject ();

        [TychoGetter ("object", "methods")]
        static AnyObject GetMethods (AnyObject self, params AnyObject [] arguments) {
            PrototypeObject prot = self.Expect<PrototypeObject> ();
            AnyObject methods = RuntimeModule.CreateSet ();

            foreach (KeyValuePair<AnyObject, Slot> pair in prot.Slots) {
                if (pair.Value is MethodSlot) {
                    methods.Add (pair.Key);
                }
            }

            if (!prot.Prototype.IsNull && prot.Prototype.HasProperty (Symbols.RuntimeMethods)) {
                foreach (AnyObject item in prot.Prototype.GetProperty (Symbols.RuntimeMethods)) {
                    methods.Add (item);
                }
            }

            return methods;
        }

        [TychoGetter ("object", "object-references")]
        static AnyObject GetObjectReferences (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.CreateSet (self.ObjectReferences);
        }

        [TychoGetter ("object", "properties")]
        static AnyObject GetProperties (AnyObject self, params AnyObject [] arguments) {
            PrototypeObject prot = self.Expect<PrototypeObject>();
            AnyObject props = RuntimeModule.CreateSet ();

            foreach (KeyValuePair<AnyObject, Slot> pair in prot.Slots) {
                if (pair.Value is FieldSlot || pair.Value is PropertySlot) {
                    props.Add (pair.Key);
                }
            }

            if (!prot.Prototype.IsNull && prot.Prototype.HasProperty (Symbols.RuntimeProperties)) {
                foreach (AnyObject item in prot.Prototype.GetProperty (Symbols.RuntimeProperties)) {
                    props.Add (item);
                }
            }

            return props;
        }

        [TychoMethod2 ("object", "equals")]
        public static bool Equals (AnyObject self, AnyObject arg) {
            return self.Equals (arg);
        }

        [TychoMethodSchema ("object", "specialize")]
        public static AnyObject NewSchema = new AnySchemaObject ();
        [TychoMethod ("object", "specialize")]
        public static AnyObject New (AnyObject self, params AnyObject [] arguments) {
            AnyObject prototype = arguments[0];
            AnyObject obj;
            
            if (prototype.IsNull) {
                obj = RuntimeModule.CreatePrototype ();
            } else {
                obj = RuntimeModule.CreatePrototype (prototype);
            }

            for (int n = 1; n < arguments.Length; n++) {
                AddMember (obj, arguments[n]);
            }

            return obj;
        }

        private static void AddMember (AnyObject obj, AnyObject member) {
            if (member.HasProperty (Symbols.RuntimeMethod)) {
                obj.AddMethod (member.GetProperty (Symbols.RuntimeName), member.GetProperty (Symbols.RuntimeMethod));
            } else if (member.HasProperty (Symbols.RuntimeGetter)) {
                obj.AddPropertyGetter (member.GetProperty (Symbols.RuntimeName), member.GetProperty (Symbols.RuntimeGetter), true);
            } else if (member.HasProperty (Symbols.RuntimeSetter)) {
                obj.AddPropertySetter (member.GetProperty (Symbols.RuntimeName), member.GetProperty (Symbols.RuntimeSetter), true);
            } else if (member.HasProperty (Symbols.RuntimeField)) {
                obj.SetProperty (member.GetProperty (Symbols.RuntimeName), member.GetProperty (Symbols.RuntimeField));
            } else {
                throw new TychoException ("member not understood: " + member);
            }
        }

        [TychoMethod2 ("object", "add-field")]
        public static void AddField (PrototypeObject self, AnyObject name, AnyObject value, bool overrideField) {
            self.AddField (name, value, overrideField);
        }

        [TychoMethod2 ("object", "add-property-getter")]
        public static void AddPropertyGetter (PrototypeObject self, AnyObject name, AnyObject getter, bool overrideMember) {
            self.AddPropertyGetter (name, getter, overrideMember);
        }

        [TychoMethod2 ("object", "add-property-setter")]
        public static void AddPropertySetter (PrototypeObject self, AnyObject name, AnyObject setter, bool overrideMember) {
            self.AddPropertySetter (name, setter, overrideMember);
        }

        [TychoMethod2 ("object", "add-method")]
        public static void AddMethod (PrototypeObject self, AnyObject name, AnyObject setter) {
            self.AddMethod (name, setter);
        }

        [TychoMethodSchema ("object", "add-protocols")]
        public static AnyObject AddProtocolsSchema = new AnySchemaObject ();
        [TychoMethod ("object", "add-protocols")]
        public static AnyObject AddProtocols (AnyObject self, params AnyObject [] protocols) {
            AnyObject protocolsImplemented;

            if (self.HasProperty (Symbols.RuntimeProtocolsImplemented)) {
                protocolsImplemented = self.GetProperty (Symbols.RuntimeProtocolsImplemented);
                foreach (var protocol in protocols) {
                    protocolsImplemented.Add (protocol);
                }
            } else {
                protocolsImplemented = RuntimeModule.CreateSet (protocols);
                self.SetProperty (Symbols.RuntimeProtocolsImplemented, protocolsImplemented);
            }

            return RuntimeModule.Null;
        }

        [TychoMethod2("object", "to-string")]
        public static string ToString (AnyObject self) {
            return self.ToString ();
        }
    }
}
