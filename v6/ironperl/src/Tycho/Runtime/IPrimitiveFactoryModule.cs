using System;
using System.Collections.Generic;

namespace Tycho.Runtime {
    public interface IPrimitiveFactoryModule {
        AnyObject CreateStructure (params AnyObject [] fields);
        AnyObject CreateList (IEnumerable<AnyObject> items);
        AnyObject CreateList (params AnyObject [] items);
        AnyObject CreateInteger (int n);
        AnyObject CreateReal (double n);
        AnyObject CreateBoolean (bool b);
        AnyObject CreateString (string s);
        AnyObject CreateDateTime (DateTime d);
    }
}