using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class AspectChainObject : AnyObject {
        ISet<Symbol> _aspectNames;
        public override AnyObject ProxyTarget { get; set; }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            return ProxyTarget.InvokeMethod (self, name, arguments);
        }

        ISet<Symbol> AspectNames {
            get {
                if (_aspectNames == null) {
                    _aspectNames = new Set<Symbol> ();
                }

                return _aspectNames;
            }
            set {
                _aspectNames = value;
            }
        }

        public void AddAspect (Symbol name, AnyObject aspect) {
            if (AspectNames.Add (name)) {
                aspect.ProxyTarget = ProxyTarget;
                ProxyTarget = aspect;
            }
        }

        public bool HasAspect (Symbol name) {
            return AspectNames.Contains (name);
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                return ProxyTarget;
            }
        }
    }
}
