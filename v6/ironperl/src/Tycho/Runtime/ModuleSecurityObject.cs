using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class ModuleSecurityObject : AnyObject {
        Namespace AllowedModule;
        AnyObject Modules;

        public ModuleSecurityObject (Namespace allowedModule, AnyObject modules) {
            AllowedModule = allowedModule;
            Modules = modules;
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            if (name == Symbols.RuntimeSetVariable) {
                SetVariable (self, arguments [0], arguments [1].ExpectValue<int> (), arguments [2]);
                return arguments [2];
            } else {
                return Modules.InvokeMethod (self, name, arguments);
            }
        }

        public override void SetVariable (AnyObject self, AnyObject name, int frameIndex, AnyObject value) {
            if (name is Symbol && ((Symbol) name).Namespace == AllowedModule) {
                Modules.SetVariable (self, name, frameIndex, value);
            } else {
                throw new TychoException ("cannot set variable " + name + ", this module " + AllowedModule + " has no write access to other modules");
            }
        }
    }
}
