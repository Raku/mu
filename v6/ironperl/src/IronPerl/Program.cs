using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Tycho.Runtime;
using System.Reflection.Emit;
using System.Reflection;
using Tycho.Language;
using NUnit.Framework;
using Goletas.Collections;
using System.Extensions;

namespace IronPerl {
    using C = System.Console;
    using PreloadedNativeModuleLoader = Tycho.Native.PreloadedNativeModuleLoader;
    public class Program {

        public static void Main (string[] args) {
            TopLevel toplevel = new TopLevel (Namespaces.User);
//            toplevel.AddModule (Namespaces.Tycho.GetNamespace ("native"),
//                new AssemblyModuleLoader (Namespaces.Tycho.GetNamespace ("native"), Assembly.Load ("Tycho.Native")));
//            List<string> assemblies = GetSystemAssemblies ();
//            PreloadedNativeModuleLoader nativeLoader = new PreloadedNativeModuleLoader (assemblies);
//            toplevel.AddModule (Namespace.Parse ("system"), nativeLoader);
//            toplevel.AddModule (Namespace.Parse ("microsoft"), nativeLoader);
            
            using (new ThreadContext ()) {
                ThreadContext.Current.SetProperty (Symbols.RuntimePrintStream, new NativeObject<TextWriter> (Console.Out));
            
            
                AnyObject program = ExpressionLanguage.CompileOperation (source, fileName, StackFrame, ModuleManifest);
                
                AnyObject result = program.Invoke ();
                C.WriteLine(result);
            }
            
            C.WriteLine("finis");
            C.ReadKey(true);
        }


/*        private static List<string> GetSystemAssemblies () {
            return new List<string> {
                                        "System",
                                        "mscorlib",
                                        "System.Xml",
                                        "System.Data",
//                                        "System.Web",
//                                        "System.Drawing",
//                                        "System.Windows.Forms",
                                        "System.Core",
//                                        "System.ServiceModel",
//                                        "System.Workflow.ComponentModel",
//                                        "System.Workflow.Runtime",
//                                        "System.Workflow.Activities",
//                                        "WindowsBase",
//                                        "PresentationCore",
//                                        "PresentationFramework"
                                    };*/
        }









        public static int Plus3 (int a, int b, bool c) {
            return c? a + b: a - b;
        }

        public static int Plus (int a, int b) {
            return a + b;
        }

        public static void Test1 (string[] args) {
            NativeFunctionDelegate plus = NativeMethodLoader.CreateNativeMethod (typeof (Program).GetMethod ("Plus3"));
            AnyObject r = plus (4, 12, false);
            Assert.AreEqual (-8, r.ExpectValue<int> ());
            C.WriteLine("ok");
        }
    }
}
