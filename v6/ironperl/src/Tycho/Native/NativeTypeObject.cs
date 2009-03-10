using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using Tycho.Runtime;
using Tycho.Utilities;
using Tycho.Language;

namespace Tycho.Native {
    public class NativeTypeObject : NoNamespacePrototypeObject {
        private bool Loaded;

        public Type Type { get; private set; }

        public NativeTypeObject (Type type) : base (RuntimeModule.Object, "native " + type.Name) {
            Type = type;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            LazyLoad ();
            return base.InvokeMethod (self, name, arguments);
        }

        public override AnyObject GetProperty (AnyObject self, AnyObject name) {
            LazyLoad ();
            return base.GetProperty (self, name);
        }

        public override void SetProperty (AnyObject self, AnyObject name, AnyObject value) {
            LazyLoad ();
            base.SetProperty (self, name, value);
        }

        private void LazyLoad () {
            if (!Loaded) {
                if (!Type.IsGenericTypeDefinition) {
                    NativeMemberBuilder memberBuilder = new NativeMemberBuilder (Type, true);
                    memberBuilder.SetUpProperties (this);
                    memberBuilder.SetUpMethods (this);

                    AddMethod (Symbols.RuntimeNew, new NativeMethod (new AnySchemaObject (), CreateNew));
                }

                AddMethod (Symbols.RuntimeMatch, new NativeMethod (new AnySchemaObject (), PrototypeMatch));

                Loaded = true;
            }
        }

        private static AnyObject PrototypeMatch (AnyObject self, AnyObject[] arguments) {
            return self.Match (arguments[0], arguments[1]);
        }

        private PrototypeObject CachedInstancePrototype;
        public PrototypeObject InstancePrototype {
            get {
                if (CachedInstancePrototype == null) {
                    CachedInstancePrototype = new NoNamespacePrototypeObject (this);
                    NativeMemberBuilder memberBuilder = new NativeMemberBuilder (Type, false);
                    memberBuilder.SetUpMethods (CachedInstancePrototype);
                    memberBuilder.SetUpProperties (CachedInstancePrototype);
                }

                return CachedInstancePrototype;
            }
        }

        private AnyObject CreateNativeObject (object instance) {
            return new NativeObject (instance, InstancePrototype);
        }

        internal AnyObject CreateNew (AnyObject self, params AnyObject [] constructorArguments) {
            try {
                object [] nativeArgs = (from arg in constructorArguments select NativeObjectConversion.ConvertToNative (arg)).ToArray ();
                object instance = Activator.CreateInstance (Type, nativeArgs);

                return CreateNativeObject (instance);
            } catch (Exception e) {
                throw new TychoException (String.Format ("could not create native type {0} from assembly {1}", Type.Name, Type.Assembly.FullName), e);
            }
        }

        public override bool Match (AnyObject results, params AnyObject [] obj) {
            if (obj[0] is NativeObject) {
                var native = obj[0] as NativeObject;
                return Type.IsAssignableFrom (native.Object.GetType ());
            } else {
                return false;
            }
        }
    }
}
