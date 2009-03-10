using System;
using Tycho.Runtime;

namespace Tycho.Native {
    class NativeObject : PrototypeObject {
        public object Object { get; private set; }

        public NativeObject (object obj, AnyObject prototype) : base (prototype) {
            Object = obj;
        }

        public override string ToString () {
            return Object.ToString ();
        }

        public override void Dispose () {
            if (Object is IDisposable) {
                ((IDisposable) Object).Dispose ();
            }
        }
    }
}