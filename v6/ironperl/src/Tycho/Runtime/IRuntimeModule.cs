using System;
using System.Collections.Generic;
using Tycho.Parser.Tokens;

namespace Tycho.Runtime {
    public interface IRuntimeModule {
        AnyObject Integer { get; }
        AnyObject Real { get; }
        AnyObject Boolean { get; }
        AnyObject String { get; }
        AnyObject DateTime { get; }
        AnyObject Operation { get; }
        AnyObject Closure { get; }
        AnyObject Module { get; }
        AnyObject ModuleFrame { get; }
        AnyObject Dictionary { get; }
        AnyObject StackFrame { get; }
        AnyObject DynamicStackFrame { get; }
        AnyObject List { get; }
        AnyObject Set { get; }
        AnyObject Structure { get; }
        AnyObject Object { get; }
        AnyObject Symbol { get; }
        AnyObject Constructor { get; }
        AnyObject Null { get; }

        AnyObject CreateInteger (int n);
        AnyObject CreateReal (double n);
        AnyObject CreateBoolean (bool b);
        AnyObject CreateString (string s);
        AnyObject CreateDateTime (DateTime d);

        AnyObject CreateStructure (params AnyObject [] fields);
        AnyObject CreateList (IEnumerable<AnyObject> items);
        AnyObject CreateSet (IEnumerable<AnyObject> items);
        AnyObject CreateDictionary (params AnyObject [] entries);
        AnyObject CreateClosure (ByteCode [] code, AnyObject [] constants, AnyObject parametersSchema, AnyObject context, SourceLocation sloc);
        AnyObject CreateModule (Namespace ns);
        AnyObject CreatePrototype ();
        AnyObject CreatePrototype (AnyObject parent);
        AnyObject CreateNative<T>(T native);
        AnyObject CreateProtocol ();
    }
}