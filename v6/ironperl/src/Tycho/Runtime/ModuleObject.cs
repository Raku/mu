using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;
using IronPerl;

namespace Tycho.Runtime {
    public class ModuleObject : InstanceObject {
        public Namespace Namespace { get; private set; }
        Dictionary<Symbol, AnyObject> ModuleVariables;

        public ModuleObject (AnyObject prototype, Namespace ns) : base (prototype) {
            Namespace = ns;
            ModuleVariables = new Dictionary<Symbol, AnyObject> ();
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> refs = new HashSet<AnyObject> (ModuleVariables.Values);
                foreach (Symbol s in ModuleVariables.Keys) {
                    refs.Add (s);
                }

                return refs;
            }
        }

        public override AnyObject GetVariable (AnyObject self, AnyObject name, int frameIndex) {
            if (frameIndex != 0) {
                throw new TychoException ("looking up variable beyond module");
            }

            return this [name];
        }

        public override void SetVariable (AnyObject self, AnyObject name, int frameIndex, AnyObject value) {
            if (frameIndex != 0) {
                throw new TychoException ("looking up variable beyond module");
            }

            this [name] = value;
        }

        public override AnyObject this [AnyObject name] {
            get {
                AnyObject var;
                if (ModuleVariables.TryGetValue (name.Expect<Symbol> (), out var)) {
                    return var;
                } else {
                    throw TychoException.NoSuchVariable (name);
                }
            }
            set {
                Symbol s = name.Expect<Symbol> ();
                Console.WriteLine("setting " + s);
                throw new Exception("breakpoint");
                if (s.Namespace != Namespace) {
                    throw new TychoException ("cannot set variable of namespace " + s.Namespace + " in module of namespace " + Namespace);
                }

                ModuleVariables [s] = value;
            }
        }

        AnyObject GetVariables () {
            return RuntimeModule.CreateSet (ModuleVariables.Keys.Select (k => (AnyObject) k));
        }

        public override string ToString (HashSet<AnyObject> done) {
            return "module " + Namespace;
        }

        [TychoMethodSchema ("module", "namespace")]
        [TychoMethodSchema ("module", "variables")]
        [TychoMethodSchema ("module", "outer-scope")]
        static AnyObject GetNamespaceSchema = new AnySchemaObject ();

        [TychoGetter ("module", "namespace")]
        static AnyObject GetNamespace (AnyObject self, params AnyObject [] arguments) {
            return self.Expect<ModuleObject> ().Namespace;
        }

        [TychoGetter ("module", "variables")]
        static AnyObject GetVariables (AnyObject self, params AnyObject [] arguments) {
            return self.Expect<ModuleObject> ().GetVariables ();
        }

        [TychoGetter ("module", "outer-scope")]
        static AnyObject GetOuterScope (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.Null;
        }

        [TychoMethod2 ("module", "index-set")]
        public static void IndexSet (AnyObject self, AnyObject index, AnyObject value) {
            self [index] = value;
        }

        [TychoMethod2 ("module", "get-variable")]
        public static AnyObject GetVariable (ModuleObject self, AnyObject name, int frameIndex) {
            return self.GetVariable ((AnyObject) self, name, frameIndex);
        }
    }
}
