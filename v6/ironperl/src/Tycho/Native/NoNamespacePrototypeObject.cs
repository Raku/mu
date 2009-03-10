using Tycho.Runtime;
using IronPerl;

namespace Tycho.Native {
    public class NoNamespacePrototypeObject : PrototypeObject {
        public NoNamespacePrototypeObject (AnyObject prototype)
            : base (prototype) {
        }

        public NoNamespacePrototypeObject (AnyObject prototype, string name)
            : base (prototype, name) {
        }

        public override AnyObject InvokeMethod (AnyObject self, AnyObject name, params AnyObject [] arguments) {
            return base.InvokeMethod (self, RemoveNamespace (name), arguments);
        }

        private AnyObject RemoveNamespace (AnyObject name) {
            Symbol symbol;
            if (name.TryCastTo (out symbol) && symbol.Namespace != Namespaces.Runtime) {
                name = Namespaces.Root.Get (symbol.Name);
            }
            return name;
        }

        public override AnyObject GetProperty (AnyObject self, AnyObject name) {
            return base.GetProperty (self, RemoveNamespace (name));
        }

        public override void SetProperty (AnyObject self, AnyObject name, AnyObject value) {
            base.SetProperty (self, RemoveNamespace (name), value);
        }
    }
}
