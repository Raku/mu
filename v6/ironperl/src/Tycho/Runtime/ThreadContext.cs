using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace Tycho.Runtime {
    public class ThreadContext : IDisposable {
        [ThreadStatic]
        static AnyObject _current;

        public ThreadContext () {
            _current = new ThreadContextObject (_current);
        }

        public static AnyObject Current {
            get {
                return _current;
            }
        }

        public void Dispose () {
            if (_current != null) {
                _current = _current.ProxyTarget;
            }
        }
    }
}
