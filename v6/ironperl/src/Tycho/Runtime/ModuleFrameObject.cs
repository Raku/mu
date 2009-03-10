using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class ModuleFrameObject : InstanceObject {
        public Dictionary<Namespace, AnyObject> ModuleLookup { get; private set; }

        public ModuleFrameObject (AnyObject prototype) : base (prototype) {
            ModuleLookup = new Dictionary<Namespace, AnyObject> ();
        }

        public bool AddModule (AnyObject module) {
            Namespace ns = module.GetProperty (Symbols.RuntimeNamespace).Expect<Namespace> ();

            if (ModuleLookup.ContainsKey (ns)) {
                return false;
            } else {
                ModuleLookup.Add (ns, module);
                return true;
            }
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeGetVariable) {
                return GetVariable (self, arguments [0], arguments [1].ExpectValue<int> ());
            } else if (name == Symbols.RuntimeSetVariable) {
                SetVariable (self, arguments [0], arguments [1].ExpectValue<int> (), arguments [2]);
                return arguments [2];
            } else if (name == Symbols.RuntimeAddModule) {
                return RuntimeModule.CreateBoolean (AddModule (arguments [0]));
            } else {
                return base.InvokeMethod (self, name, arguments);
            }
        }

        AnyObject GetModule (Namespace ns) {
            AnyObject module;
            if (ModuleLookup.TryGetValue (ns, out module)) {
                return module;
            } else {
                throw new TychoException ("no such module " + ns + " loaded");
            }
        }

        public override void SetVariable (AnyObject self, AnyObject name, int frameIndex, AnyObject value) {
            Symbol symbol = (Symbol) name;

            AnyObject module = GetModule (symbol.Namespace);
            module.SetVariable (module, name, frameIndex, value);
        }

        public override AnyObject GetVariable (AnyObject self, AnyObject name, int frameIndex) {
            Symbol symbol = (Symbol) name;

            AnyObject module = GetModule (symbol.Namespace);
            return module.GetVariable (module, name, frameIndex);
        }

        public IEnumerable<AnyObject> Modules {
            get {
                return ModuleLookup.Values;
            }
        }

        public override AnyObject Variables {
            get {
                return GetVariables ();
            }
        }

        private AnyObject GetVariables () {
            AnyObject vars = RuntimeModule.CreateSet ();

            foreach (AnyObject module in Modules) {
                foreach (AnyObject var in module.Variables) {
                    vars.Add (var);
                }
            }

            return vars;
        }

        [TychoMethodSchema ("module-frame", "outer-scope")]
        static AnyObject GetNamespaceSchema = new AnySchemaObject ();
        
        [TychoGetter2 ("module-frame", "variables")]
        public static AnyObject GetVariables (ModuleFrameObject self) {
            return self.GetVariables ();
        }

        [TychoGetter ("module-frame", "outer-scope")]
        static AnyObject GetOuterScope (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.Null;
        }
    }
}
