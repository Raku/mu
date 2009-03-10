using System;
using System.Collections.Generic;
using Tycho.Compiler;
using Tycho.Language;

namespace Tycho.Runtime {
    public class ModuleManifest : IModuleScopeLoader {
        public Dictionary<Namespace, IModuleLoader> ModuleLoaders { get; private set; }
        private Dictionary<Namespace, AnyObject> Modules;
        private ModuleFrameObject ModuleFrame;

        public ModuleManifest (ModuleFrameObject moduleFrame) {
            ModuleLoaders = new Dictionary<Namespace, IModuleLoader> ();
            Modules = new Dictionary<Namespace, AnyObject> ();
            ModuleFrame = moduleFrame;
        }

        SymbolScope IModuleScopeLoader.LoadModule (Namespace ns) {
            AnyObject module = FindModule (ns);

            if (module != null) {
                return ExpressionLanguage.BuildScopeRecursive (module);
            } else {
                return null;
            }
        }

        AnyObject FindModule (Namespace ns) {
            AnyObject module;
            if (Modules.TryGetValue (ns, out module)) {
                return module;
            } else {
                return FindModule (ns, ns);
            }
        }

        AnyObject FindModule (Namespace baseNamespace, Namespace ns) {
            IModuleLoader moduleLoader;
            if (ModuleLoaders.TryGetValue (baseNamespace, out moduleLoader)) {
                AnyObject module = moduleLoader.LoadModule (ns, SubArray (ns.Path, baseNamespace.Path.Length), new NonRecursiveModuleLoader (ns, this));
                Modules.Add (ns, module);
                ModuleFrame.AddModule (module);
                return module;
            } else if (baseNamespace.Parent != null) {
                return FindModule (baseNamespace.Parent, ns);
            } else {
                return null;
            }
        }

        T [] SubArray<T> (T [] array, int startIndex) {
            T[] newArray = new T[array.Length - startIndex];
            Array.Copy (array, startIndex, newArray, 0, newArray.Length);
            return newArray;
        }
    }

    public class NonRecursiveModuleLoader : IModuleScopeLoader {
        private Namespace NamespaceToBlock;
        private IModuleScopeLoader Loader;

        public NonRecursiveModuleLoader (Namespace namespaceToBlock, IModuleScopeLoader loader) {
            NamespaceToBlock = namespaceToBlock;
            Loader = loader;
        }

        public SymbolScope LoadModule (Namespace ns) {
            if (ns != NamespaceToBlock) {
                return Loader.LoadModule (ns);
            } else {
                return null;
            }
        }
    }
}