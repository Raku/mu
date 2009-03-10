using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Utilities;
using IronPerl;

namespace Tycho.Native {
    public static class NativeModule {
        public static Namespace NativeNamespace;
        public static Symbol NativeSymbol;
        public static Symbol NativeTypeSymbol { get; private set; }
        public static AnyObject NativeType { get; private set; }

        static NativeModule () {
            NativeType = RuntimeModule.CreatePrototype ();
            NativeNamespace = Namespaces.CORE.GetNamespace ("native");
            NativeTypeSymbol = NativeNamespace.Get ("native-type");
            NativeSymbol = NativeNamespace.Get ("native");
        }

        [TychoModuleLoad]
        static void LoadModule (AnyObject module) {
            RuntimeModule.ConvertToRuntime = NativeObjectConversion.ConvertToRuntime;
            module [NativeTypeSymbol] = NativeType;
        }
    }
}
