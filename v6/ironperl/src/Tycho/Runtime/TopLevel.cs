using System;
using System.Linq;
using System.Text;
using Tycho.Parser.Tokens;
using Tycho.Parser;
//using Tycho.Lexer;
using System.IO;
using System.Reflection;
using Tycho.Utilities;
using Tycho.Language;

namespace Tycho.Runtime {
    /*public class TopLevel : ITopLevel {
        AnyObject StackFrame;
        //ExpressionLanguage ExpressionLanguage;
        public ModuleManifest ModuleManifest { get; private set; }
        private static AnyObject CachedRuntimeAssemblyModule;
        public ModuleFrameObject ModuleFrame { get; private set; }

        public static AnyObject RuntimeAssemblyModule {
            get {
                if (CachedRuntimeAssemblyModule == null) {
                    CachedRuntimeAssemblyModule = AssemblyModuleLoader.LoadAssembly (Namespaces.Runtime, typeof (TopLevel).Assembly);
                }
                return CachedRuntimeAssemblyModule;
            }
        }

        public TopLevel (Namespace defaultNamespace, AnyObject stackFrame) {
            ModuleFrame = new ModuleFrameObject (RuntimeModule.ModuleFrame);

            ModuleManifest = new ModuleManifest (ModuleFrame);
            ModuleManifest.ModuleLoaders.Add (Namespaces.Runtime, new ImmediateModuleLoader (RuntimeAssemblyModule));

            ModuleSecurityObject moduleAccess = new ModuleSecurityObject (defaultNamespace, ModuleFrame);
            if (stackFrame != null) {
                StackFrame = stackFrame;
                StackFrame.OuterScope = moduleAccess;
            } else {
                StackFrame = new StackFrameObject (RuntimeModule.StackFrame, moduleAccess);
            }

            //ExpressionLanguage = new ExpressionLanguage (moduleAccess, defaultNamespace);
        }

        public TopLevel (Namespace defaultNamespace) : this (defaultNamespace, null) { }

        public TopLevel () : this (Namespaces.User) { }
        
        public AnyObject Evaluate (string source) {
            return Evaluate (source, null);
        }
        
        public AnyObject this [AnyObject name] {
            get { return StackFrame [name]; }
            set { StackFrame [name] = value; }
        }

        public AnyObject Evaluate(string source, string fileName) {
            return ExpressionLanguage.CompileOperation (source, fileName, StackFrame, ModuleManifest).Invoke ();
        }
        
        public AnyObject Compile(string source) {
            return ExpressionLanguage.CompileOperation(source, null, StackFrame, ModuleManifest);
        }
        
        public AnyObject EvaluateFile (string fileName) {
            return Evaluate (File.ReadAllText (fileName), fileName);
        }
        
        public void AddModule (Namespace ns, IModuleLoader moduleLoader) {
            ModuleManifest.ModuleLoaders.Add (ns, moduleLoader);
        }
    }*/
}
