using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class TychoNullPointerException : TychoException {
        public TychoNullPointerException () : base ("null pointer exception") { }
    }
}
