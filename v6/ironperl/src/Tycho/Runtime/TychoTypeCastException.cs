using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class TychoTypeCastException : TychoException {
        public Type Type { get; private set; }

        public TychoTypeCastException (Type type) : base ("expected " + type) {
            Type = type;
        }
    }
}
