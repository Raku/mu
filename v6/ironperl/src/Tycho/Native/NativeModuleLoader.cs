using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using Tycho.Compiler;
using Tycho.Runtime;

namespace Tycho.Native {
    public class NativeModuleLoader : IModuleLoader {
        private Dictionary<Namespace, AnyObject> Modules;
        private Dictionary<Namespace, List<string>> AssemblyMap;

        public NativeModuleLoader (Dictionary<Namespace, List<string>> assemblyMap) {
            Modules = new Dictionary<Namespace, AnyObject>();
            AssemblyMap = assemblyMap;
        }

        public void LoadAssembly (Assembly assembly) {
            foreach (Type type in assembly.GetTypes ()) {
                if (type.IsPublic) {
                    AddType (type);
                }
            }
        }

        private void AddType (Type type) {
            Namespace ns = GetNamespace (type.Namespace);
            AnyObject module = GetModule (ns);
            module[ns.Get(NameConverter.Convert (type.Name))] = new NativeTypeObject (type);
        }

        private Namespace GetNamespace (string ns) {
            return Namespace.Parse (NameConverter.ConvertNamespace (ns));
        }

        private AnyObject GetModule (Namespace ns) {
            AnyObject module;
            if (!Modules.TryGetValue (ns, out module)) {
                module = RuntimeModule.CreateModule (ns);
                Modules.Add (ns, module);
            }

            return module;
        }

        public AnyObject LoadModule (Namespace ns, string [] modulePath, IModuleScopeLoader moduleLoader) {
            TryLoadAssembly (ns);
            return GetModule (ns);
        }

        private void TryLoadAssembly (Namespace ns) {
            List<string> assemblies;
            if (AssemblyMap.TryGetValue (ns, out assemblies)) {
                foreach (string assemblyName in assemblies) {
                    LoadAssembly (Assembly.Load (assemblyName));
                }
            }
        }
    }
}
