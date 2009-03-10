using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using Tycho.Compiler;
using Tycho.Runtime;
using Tycho.Utilities;
using System.Reflection.Emit;

namespace IronPerl {
    using NativeMethodLoader = Tycho.Language.NativeMethodLoader;

    public class AssemblyModuleLoader : IModuleLoader {
        struct NameObject {
            string MethodName, Object, SchemaName;

            public NameObject (string methodName, string objectName, string schemaName) {
                MethodName = methodName;
                Object = objectName;
                SchemaName = schemaName;
            }
        }

        private Namespace Namespace;
        private Assembly Assembly;

        public AssemblyModuleLoader (Namespace ns, Assembly assembly) {
            Namespace = ns;
            Assembly = assembly;
        }

        public AnyObject LoadModule (Namespace ns, string [] modulePath, IModuleScopeLoader moduleLoader) {
            return LoadAssembly (Namespace, Assembly);
        }

        static BindingFlags AssemblyLoadFlags = BindingFlags.Static | BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.NonPublic;

        static void CheckPublic (MethodInfo method) {
            if (!method.IsPublic) throw new TychoException ("method " + method.ReflectedType.Name + "." + method.Name + " must be public");
        }

        public static AnyObject LoadAssembly (Namespace ns, Assembly assembly) {
            AnyObject module = RuntimeModule.CreateModule (ns);

            LoadTypes(assembly.GetTypes(), ns, module);

            return module;
        }

        public static void LoadType(Type type, Namespace ns, AnyObject module) {
            LoadTypes(new[] {type}, ns, module);
        }

        public static void LoadTypes(IEnumerable<Type> types, Namespace ns, AnyObject module) {
            foreach (Type type in types) {
                LoadObjects(type, ns, module);
            }

            foreach (Type type in types) {
                LoadObjectMembers(type, ns, module);
            }
        }

        public static void LoadObjects(Type type, Namespace ns, AnyObject module) {
            foreach (FieldInfo field in type.GetFields (AssemblyLoadFlags)) {
                foreach (TychoObjectAttribute attr in field.GetCustomAttributes (typeof (TychoObjectAttribute), false)) {
                    if (typeof (AnyObject).IsAssignableFrom(field.FieldType)) {
                        module[ns.Get(attr.Name)] = (AnyObject) field.GetValue(null);
                    } else {
                        throw new ApplicationException("fields marked with [TychoObject] must be of AnyObject type");
                    }
                }
            }
            foreach (MethodInfo method in type.GetMethods (AssemblyLoadFlags)) {
                foreach (TychoModuleLoadAttribute attr in method.GetCustomAttributes (typeof (TychoModuleLoadAttribute), false)) {
                    method.Invoke (type, new object [1] { module });
                }
            }
        }

        public static void LoadObjectMembers(Type type, Namespace ns, AnyObject module) {
            Dictionary<NameObject, AnyObject> methodSchemas = new Dictionary<NameObject, AnyObject> ();
            Dictionary<string, AnyObject> functionSchemas = new Dictionary<string, AnyObject> ();

            foreach (FieldInfo field in type.GetFields (AssemblyLoadFlags)) {
                foreach (TychoMethodSchemaAttribute attr in field.GetCustomAttributes (typeof (TychoMethodSchemaAttribute), false)) {
                    methodSchemas.Add (new NameObject (attr.Name, attr.Object, attr.SchemaName), (AnyObject) field.GetValue (type));
                }
                foreach (TychoFunctionSchemaAttribute attr in field.GetCustomAttributes (typeof (TychoFunctionSchemaAttribute), false)) {
                    functionSchemas.Add (attr.Name, (AnyObject) field.GetValue (type));
                }
            }

            foreach (MethodInfo method in type.GetMethods (AssemblyLoadFlags)) {
                foreach (TychoMethodAttribute attr in method.GetCustomAttributes (typeof (TychoMethodAttribute), false)) {
                    AnyObject schema = methodSchemas [new NameObject (attr.Name, attr.Object, attr.SchemaName)];
                    NativeMethod nativeMethod = new NativeMethod (schema, (NativeMethodDelegate) Delegate.CreateDelegate (typeof (NativeMethodDelegate), method));

                    module [ns.Get (attr.Object)].AddMethod (ns.Get (attr.Name), nativeMethod);
                }
                foreach (TychoGetterAttribute attr in method.GetCustomAttributes (typeof (TychoGetterAttribute), false)) {
                    AnyObject schema = methodSchemas [new NameObject (attr.Name, attr.Object, attr.SchemaName)];
                    NativeMethod nativeMethod = new NativeMethod (schema, (NativeMethodDelegate) Delegate.CreateDelegate (typeof (NativeMethodDelegate), method));

                    module [ns.Get (attr.Object)].AddPropertyGetter (ns.Get (attr.Name), nativeMethod, false);
                }
                foreach (TychoSetterAttribute attr in method.GetCustomAttributes (typeof (TychoSetterAttribute), false)) {
                    AnyObject schema = methodSchemas [new NameObject (attr.Name, attr.Object, attr.SchemaName)];
                    NativeMethod nativeMethod = new NativeMethod (schema, (NativeMethodDelegate) Delegate.CreateDelegate (typeof (NativeMethodDelegate), method));

                    module [ns.Get (attr.Object)].AddPropertySetter (ns.Get (attr.Name), nativeMethod, false);
                }
                foreach (TychoFunctionAttribute attr in method.GetCustomAttributes (typeof (TychoFunctionAttribute), false)) {
                    AnyObject schema = functionSchemas [attr.Name];
                    NativeFunction nativeFunction = new NativeFunction (schema, (NativeFunctionDelegate) Delegate.CreateDelegate (typeof (NativeFunctionDelegate), method));

                    module [ns.Get (attr.Name)] = nativeFunction;
                }
                foreach (TychoFunction2Attribute attr in method.GetCustomAttributes (typeof (TychoFunction2Attribute), false)) {
                    CheckPublic (method);
                    AnyObject schema = NativeMethodLoader.CreateNativeSchema (method);
                    NativeFunction nativeFunction = new NativeFunction (schema, NativeMethodLoader.CreateNativeMethod (method));

                    module [ns.Get (attr.Name)] = nativeFunction;
                }
                foreach (TychoMethod2Attribute attr in method.GetCustomAttributes (typeof (TychoMethod2Attribute), false)) {
                    CheckPublic (method);
                    AnyObject schema = NativeMethodLoader.CreateNativeSchema (method);
                    NativeFunction nativeFunction = new NativeFunction (schema, NativeMethodLoader.CreateNativeMethod (method));

                    module [ns.Get (attr.Object)].AddMethod (ns.Get (attr.Name), nativeFunction);
                }
                foreach (TychoGetter2Attribute attr in method.GetCustomAttributes (typeof (TychoGetter2Attribute), false)) {
                    CheckPublic (method);
                    AnyObject schema = NativeMethodLoader.CreateNativeSchema (method);
                    NativeFunction nativeFunction = new NativeFunction (schema, NativeMethodLoader.CreateNativeMethod (method));

                    module [ns.Get (attr.Object)].AddPropertyGetter (ns.Get (attr.Name), nativeFunction, false);
                }
                foreach (TychoSetter2Attribute attr in method.GetCustomAttributes (typeof (TychoSetter2Attribute), false)) {
                    CheckPublic (method);
                    AnyObject schema = NativeMethodLoader.CreateNativeSchema (method);
                    NativeFunction nativeFunction = new NativeFunction (schema, NativeMethodLoader.CreateNativeMethod (method));

                    module [ns.Get (attr.Object)].AddPropertySetter (ns.Get (attr.Name), nativeFunction, false);
                }
            }
        }
    }
}

