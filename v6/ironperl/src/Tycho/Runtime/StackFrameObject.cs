using System.Collections.Generic;

namespace Tycho.Runtime {
    public class StackFrameObject : InstanceObject {
        public override AnyObject OuterScope { get; set; }
        private Dictionary<AnyObject, AnyObject> Frame;

        public StackFrameObject (AnyObject prototype, AnyObject outerScope) : base (prototype) {
            OuterScope = outerScope;
            Frame = new Dictionary<AnyObject, AnyObject> ();
        }

        public override AnyObject GetVariable (AnyObject self, AnyObject name, int frameIndex) {
            if (frameIndex == 0) {
                AnyObject var;
                if (Frame.TryGetValue (name, out var)) {
                    return var;
                } else {
                    throw TychoException.NoSuchVariable (name);
                }
            } else if (OuterScope != null) {
                return OuterScope.GetVariable (OuterScope, name, frameIndex - 1);
            } else {
                throw new TychoException ("stack frame parent missing for variable " + name);
            }
        }

        public override void SetVariable (AnyObject self, AnyObject name, int frameIndex, AnyObject value) {
            if (frameIndex == 0) {
                Frame [name] = value;
            } else if (OuterScope != null) {
                OuterScope.SetVariable (self, name, frameIndex - 1, value);
            } else {
                throw new TychoException ("stack frame parent missing for variable " + name);
            }
        }

        public override AnyObject this [AnyObject index] {
            get {
                return GetVariable (this, index, 0);
            }
            set {
                SetVariable (this, index, 0, value);
            }
        }

        public bool HasVariable (AnyObject name) {
            AnyObject var;
            return Frame.TryGetValue (name, out var);
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                HashSet<AnyObject> refs = new HashSet<AnyObject> (Frame.Keys);
                refs.UnionWith (Frame.Values);
                refs.Add (OuterScope);

                return refs;
            }
        }

        public override AnyObject Variables {
            get {
                return RuntimeModule.CreateSet (Frame.Keys);
            }
        }
    }
}
