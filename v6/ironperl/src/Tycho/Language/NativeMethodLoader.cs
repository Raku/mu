using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection.Emit;
using Tycho.Runtime;
using System.Reflection;

namespace Tycho.Language {
    public class NativeMethodLoader {
        public static NativeFunctionDelegate CreateNativeMethod (MethodInfo method) {
            if (!method.IsStatic && method.IsPublic) {
                throw new TychoException ("method " + method.Name + " must be public and static");
            }

            DynamicMethod wrapper = new DynamicMethod (method.Name, typeof (AnyObject), new Type [] { typeof (AnyObject []) });
            ILGenerator code = wrapper.GetILGenerator ();
            ParameterInfo [] parameters = method.GetParameters ();

            LocalBuilder result = code.DeclareLocal (typeof (int));

            for (int n = 0; n < parameters.Length; n++) {
                ConvertListedArgument (code, parameters [n].ParameterType, n);
            }

            code.Emit (OpCodes.Call, method);

            ConvertReturnValue (code, method.ReturnType);
            code.Emit (OpCodes.Ret);

            return (NativeFunctionDelegate) wrapper.CreateDelegate (typeof (NativeFunctionDelegate));
        }

        static void ConvertReturnValue (ILGenerator code, Type returnType) {
            string createMethodName;
            if (returnType == typeof (void)) {
                code.Emit (OpCodes.Ldsfld, typeof (RuntimeModule).GetField ("Null"));
            } else if (typeof (AnyObject).IsAssignableFrom (returnType)) {
                // already converted
            } else if (TryGetValueTypePrototypeFieldName (returnType, out createMethodName)) {
                code.Emit (OpCodes.Call, typeof (RuntimeModule).GetMethod (createMethodName));
            } else {
                Type nativeObject = typeof (NativeObject<>).MakeGenericType (new Type [] { returnType });
                code.Emit (OpCodes.Newobj, nativeObject.GetConstructor (new Type [] { returnType }));
            }
        }

        static bool TryGetValueTypePrototypeFieldName (Type valueType, out string createMethodName) {
            if (valueType == typeof (Boolean) || valueType.IsSubclassOf (typeof (Boolean))) {
                createMethodName = "CreateBoolean";
                return true;
            } else if (valueType == typeof (Int32) || valueType.IsSubclassOf (typeof (Int32))) {
                createMethodName = "CreateInteger";
                return true;
            } else if (valueType == typeof (Double) || valueType.IsSubclassOf (typeof (Double))) {
                createMethodName = "CreateReal";
                return true;
            } else if (valueType == typeof (String) || valueType.IsSubclassOf (typeof (String))) {
                createMethodName = "CreateString";
                return true;
            } else if (valueType == typeof (DateTime) || valueType.IsSubclassOf (typeof (DateTime))) {
                createMethodName = "CreateDateTime";
                return true;
            } else {
                createMethodName = null;
                return false;
            }
        }

        static void ConvertListedArgument (ILGenerator code, Type type, int argIndex) {
            code.Emit (OpCodes.Ldarg_0);
            code.Emit (OpCodes.Ldc_I4, argIndex);
            code.Emit (OpCodes.Ldelem_Ref);

            ConvertArgument (code, type);
        }

        static void ConvertArgument (ILGenerator code, Type type) {
            Type valueType;
            if (typeof (AnyObject).IsAssignableFrom (type)) {
                code.Emit (OpCodes.Castclass, type);
            } else if (TryConvertToValueType (type, out valueType)) {
                MethodInfo expectValue = typeof (AnyObject).GetMethod ("ExpectValue").MakeGenericMethod (type);
                code.Emit (OpCodes.Callvirt, expectValue);
            } else {
                MethodInfo expectNative = typeof (AnyObject).GetMethod ("ExpectNative").MakeGenericMethod (type);
                code.Emit (OpCodes.Callvirt, expectNative);
            }
        }

        public static AnyObject CreateNativeSchema (MethodInfo method) {
            return new ParametersSchemaObject (method.GetParameters ().Select (p => GetSchemaFromType (p.ParameterType)).ToArray ());
        }

        public static AnyObject GetSchemaFromType (Type t) {
            Type valueType;
            if (typeof (AnyObject).IsAssignableFrom (t)) {
                Type schemaType = typeof (TypeSchemaObject<>).MakeGenericType (new Type [] { t });
                return (AnyObject) Activator.CreateInstance (schemaType);
            } else if (TryConvertToValueType (t, out valueType)) {
                Type schemaType = typeof (TypeSchemaObject<>).MakeGenericType (new Type [] { valueType });
                return (AnyObject) Activator.CreateInstance (schemaType);
            } else {
                Type schemaType = typeof (NativeTypeSchemaObject<>).MakeGenericType (new Type [] { t });
                return (AnyObject) Activator.CreateInstance (schemaType);
            }
        }

        private static bool TryConvertToValueType (Type nativeType, out Type valueType) {
            if (nativeType.IsAssignableFrom (typeof (int))) {
                valueType = typeof (IntegerObject);
                return true;
            } else if (nativeType.IsAssignableFrom (typeof (double))) {
                valueType = typeof (RealObject);
                return true;
            } else if (nativeType.IsAssignableFrom (typeof (string))) {
                valueType = typeof (StringObject);
                return true;
            } else if (nativeType.IsAssignableFrom (typeof (DateTime))) {
                valueType = typeof (DateTimeObject);
                return true;
            } else if (nativeType.IsAssignableFrom (typeof (bool))) {
                valueType = typeof (BooleanObject);
                return true;
            } else {
                valueType = null;
                return false;
            }
        }
    }
}
