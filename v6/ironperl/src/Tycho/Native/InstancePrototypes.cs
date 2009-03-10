using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;

namespace Tycho.Native {
    class NativeTypeCache {
        private static Dictionary<Type, NativeTypeObject> CachedNativeTypes;

        static NativeTypeCache () {
            CachedNativeTypes = new Dictionary<Type, NativeTypeObject> ();
        }

        public static AnyObject GetNativeType (Type type) {
            NativeTypeObject typeObject;
            
            if (!CachedNativeTypes.TryGetValue (type, out typeObject)) {
                CachedNativeTypes.Add (type, typeObject = new NativeTypeObject (type));
            }

            return typeObject.InstancePrototype;
        }
    }
}
