using System;
using System.Collections.Generic;
using System.Reflection;
using Tycho.Compiler;
using Tycho.Runtime;

namespace Tycho.Native {
    public class PreloadedNativeModuleLoader : IModuleLoader {
        private List<string> Assemblies;
        private NativeModuleLoader CachedModuleLoader;

        public PreloadedNativeModuleLoader (List<string> assemblies) {
            Assemblies = assemblies;
        }

        private NativeModuleLoader ModuleLoader {
            get {
                if (CachedModuleLoader == null) {
                    CachedModuleLoader = new NativeModuleLoader (BuildNamespaceMap ());
                }

                return CachedModuleLoader;
            }
        }

        public AnyObject LoadModule (Namespace ns, string[] modulePath, IModuleScopeLoader moduleLoader) {
            return ModuleLoader.LoadModule (ns, modulePath, moduleLoader);
        }

        public IEnumerable<string> GetNamespaces (string name) {
            var namespaces = new HashSet<string> ();
            var asm = Assembly.Load (name);
            foreach (Type type in asm.GetTypes ()) {
                if (type.Namespace != null) {
                    namespaces.Add (type.Namespace);
                }
            }

            return namespaces;
        }

        public Dictionary<Namespace, List<string>> BuildNamespaceMap () {
            var assemblyMap = new Dictionary<Namespace, List<string>> ();

            foreach (var assembly in Assemblies) {
                foreach (var nativeNamespace in GetNamespaces (assembly)) {
                    Namespace ns = Namespace.Parse(NameConverter.ConvertNamespace (nativeNamespace));

                    List<string> assemblyList;
                    if (!assemblyMap.TryGetValue (ns, out assemblyList)) {
                        assemblyMap.Add (ns, assemblyList = new List<string> ());
                    }
                    assemblyList.Add (assembly);
                }
            }

            return assemblyMap;
        }
    }
}