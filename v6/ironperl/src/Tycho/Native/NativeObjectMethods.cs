using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;
using Tycho.Runtime;
using System.Reflection;

namespace Tycho.Native {
    static class NativeObjectMethods {
        [TychoMethodSchema ("native", "new")]
        static AnyObject NewNativeSchema = new UnlimitedParametersSchemaObject (2);
        [TychoMethod ("native", "new")]
        static AnyObject NewNative (AnyObject self, params AnyObject [] arguments) {
            AnyObject assemblyNameObject = arguments [0];
            string assemblyName = (assemblyNameObject.IsNull) ? null : assemblyNameObject.ExpectValue<string> ();
            string typeName = arguments [1].ExpectValue<string> ();

            try {
                AnyObject [] constructorArguments = new AnyObject [arguments.Length - 2];
                Array.ConstrainedCopy (arguments, 2, constructorArguments, 0, constructorArguments.Length);
                object [] nativeArgs = (from arg in constructorArguments select NativeObjectConversion.ConvertToNative (arg)).ToArray<object> ();
                object instance = Activator.CreateInstance (assemblyName, typeName, false, BindingFlags.Default, null, nativeArgs, null, null, null).Unwrap ();
                return NativeObjectConversion.ConvertToRuntime (instance);
            } catch (Exception e) {
                throw new TychoException (String.Format ("could not create native type {0} from assembly {1}", typeName, assemblyName), e);
            }
        }

        [TychoModuleLoad]
        static void LoadModule (AnyObject module) {
            module [NativeModule.NativeSymbol] = RuntimeModule.CreatePrototype ();
        }
    }
}
