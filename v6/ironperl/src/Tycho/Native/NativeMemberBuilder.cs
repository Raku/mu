using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using Tycho.Language;
using Tycho.Runtime;

namespace Tycho.Native {
    class NativeMemberBuilder {
        private Type Type;
        private bool IsStatic;

        public NativeMemberBuilder (Type type, bool isStatic) {
            Type = type;
            IsStatic = isStatic;
        }

        BindingFlags BindingFlags {
            get {
                if (IsStatic) {
                    return BindingFlags.Static | BindingFlags.Public | BindingFlags.FlattenHierarchy;
                } else {
                    return BindingFlags.Instance | BindingFlags.Public | BindingFlags.FlattenHierarchy;
                }
            }
        }

        class NativeMember {
            private bool IsStatic;

            public NativeMember (bool isStatic) {
                IsStatic = isStatic;
            }

            protected object GetSelf (AnyObject self) {
                return IsStatic ? null : self.Expect<NativeObject> ().Object;
            }
        }

        class FieldNativeProperty : NativeMember {
            private FieldInfo Field;

            public FieldNativeProperty (FieldInfo field, bool isStatic)
                : base (isStatic) {
                Field = field;
            }

            public AnyObject Getter (AnyObject self, params AnyObject [] arguments) {
                return NativeObjectConversion.ConvertToRuntime (Field.GetValue (GetSelf (self)));
            }

            public AnyObject Setter (AnyObject self, params AnyObject [] arguments) {
                var value = arguments [0];
                Field.SetValue (GetSelf (self), NativeObjectConversion.ConvertToNative (value));
                return value;
            }
        }

        class PropertyNativeProperty : NativeMember {
            private PropertyInfo Property;

            public PropertyNativeProperty (PropertyInfo property, bool isStatic)
                : base (isStatic) {
                Property = property;
            }

            public AnyObject Getter (AnyObject self, params AnyObject [] arguments) {
                return NativeObjectConversion.ConvertToRuntime (Property.GetValue (GetSelf (self), null));
            }

            public AnyObject Setter (AnyObject self, params AnyObject [] arguments) {
                var value = arguments [0];
                Property.SetValue (GetSelf (self), NativeObjectConversion.ConvertToNative (value), null);
                return arguments [0];
            }
        }

        class NativeMethod : NativeMember {
            private MethodInfo Method;

            public NativeMethod (MethodInfo method, bool isStatic)
                : base (isStatic) {
                Method = method;
            }

            public AnyObject Invoke (AnyObject self, params AnyObject [] arguments) {
                object [] nativeArgs = (from arg in arguments select NativeObjectConversion.ConvertToNative (arg)).ToArray<object> ();
                return NativeObjectConversion.ConvertToRuntime (Method.Invoke (GetSelf (self), nativeArgs));
            }
        }

        public void SetUpMethods (PrototypeObject obj) {
            foreach (MethodInfo method in Type.GetMethods (BindingFlags)) {
                if (!method.IsSpecialName && !method.IsGenericMethodDefinition && ContainsValidParameters (method)) {
                    AnyObject schema = BuildSchema (method);
                    obj.AddMethod (Symbol.Parse (NameConverter.Convert (method.Name)), new Runtime.NativeMethod (schema, new NativeMethod (method, IsStatic).Invoke));
                }
            }
        }

        private static bool ContainsValidParameters (MethodInfo method) {
            return !method.GetParameters ().Any (p => p.ParameterType.IsByRef || p.ParameterType.IsPointer || p.ParameterType == typeof (TypedReference));
        }

        private static AnyObject BuildSchema (MethodInfo method) {
            List<AnyObject> parameters = new List<AnyObject> ();
            parameters.Add (new AnySchemaObject ());
            parameters.AddRange (method.GetParameters ().Select (p => NativeMethodLoader.GetSchemaFromType (p.ParameterType)));

            return new ParametersSchemaObject (parameters.ToArray ());
        }

        public void SetUpProperties (PrototypeObject obj) {
            foreach (FieldInfo field in Type.GetFields (BindingFlags)) {
                FieldNativeProperty nativeField = new FieldNativeProperty (field, IsStatic);
                try {
                    obj.AddPropertyGetter (Symbol.Parse (NameConverter.Convert (field.Name)), new Runtime.NativeMethod (new AnySchemaObject (), nativeField.Getter), false);
                } catch (MemberAlreadyDefinedException) {
                }
                try {
                    obj.AddPropertySetter (Symbol.Parse (NameConverter.Convert (field.Name)), new Runtime.NativeMethod (new AnySchemaObject (), nativeField.Setter), false);
                } catch (MemberAlreadyDefinedException) {
                }
            }

            foreach (PropertyInfo property in Type.GetProperties (BindingFlags)) {
                PropertyNativeProperty nativeProperty = new PropertyNativeProperty (property, IsStatic);
                if (property.CanRead) {
                    try {
                        obj.AddPropertyGetter (Symbol.Parse (NameConverter.Convert (property.Name)), new Runtime.NativeMethod (new AnySchemaObject (), nativeProperty.Getter), false);
                    } catch (MemberAlreadyDefinedException) {
                    }
                }
                if (property.CanWrite) {
                    try {
                        obj.AddPropertySetter (Symbol.Parse (NameConverter.Convert (property.Name)), new Runtime.NativeMethod (new AnySchemaObject (), nativeProperty.Setter), false);
                    } catch (MemberAlreadyDefinedException) {
                    }
                }
            }
        }
    }
}
