using System;
using Tycho.Runtime;

namespace Tycho.Native {
    public static class NativeObjectConversion {
        public static object ConvertToNative (AnyObject runtimeObject) {
            object nativeObject;
            if (TryConvertValueObjectToNative (runtimeObject, out nativeObject)) {
                return nativeObject;
            } else if (runtimeObject is NativeObject) {
                return ((NativeObject) runtimeObject).Object;
            } else if (runtimeObject is NullObject) {
                return null;
            } else {
                throw new TychoException (String.Format ("object `{0}' cannot be converted", runtimeObject));
            }
        }

        public static AnyObject ConvertToRuntime (object nativeObject) {
            AnyObject runtimeObject;

            if (nativeObject == null) {
                return RuntimeModule.Null;
            } else if (TryToConvertNativeValueToRuntime (nativeObject, out runtimeObject)) {
                return runtimeObject;
            } else {
                Type nativeObjectType = nativeObject.GetType ();

                if (nativeObjectType == typeof (Type)) {
                    return NativeTypeCache.GetNativeType (nativeObjectType);
                } else {
                    return new NativeObject (nativeObject, NativeTypeCache.GetNativeType (nativeObjectType));
                }
            }
        }

        public static bool TryConvertValueObjectToNative (AnyObject runtimeObject, out object nativeObject) {
            if (runtimeObject is IntegerObject) {
                nativeObject = (runtimeObject as IntegerObject).Value;
                return true;
            } else if (runtimeObject is RealObject) {
                nativeObject = (runtimeObject as RealObject).Value;
                return true;
            } else if (runtimeObject is StringObject) {
                nativeObject = (runtimeObject as StringObject).Value;
                return true;
            } else if (runtimeObject is BooleanObject) {
                nativeObject = (runtimeObject as BooleanObject).Value;
                return true;
            } else if (runtimeObject is DateTimeObject) {
                nativeObject = (runtimeObject as DateTimeObject).Value;
                return true;
            } else {
                nativeObject = null;
                return false;
            }
        }

        private static bool TryToConvertNativeValueToRuntime (object nativeObject, out AnyObject runtimeObject) {
            if (nativeObject is int) {
                runtimeObject = (int) nativeObject;
                return true;
            } else if (nativeObject is double) {
                runtimeObject = (double) nativeObject;
                return true;
            } else if (nativeObject is string) {
                runtimeObject = (string) nativeObject;
                return true;
            } else if (nativeObject is bool) {
                runtimeObject = (bool) nativeObject;
                return true;
            } else if (nativeObject is DateTime) {
                runtimeObject = (DateTime) nativeObject;
                return true;
            } else {
                runtimeObject = null;
                return false;
            }
        }
    }
}