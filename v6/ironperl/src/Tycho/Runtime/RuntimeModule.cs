using System;
using System.Collections.Generic;

namespace Tycho.Runtime {
    public class RuntimeModule {
        static RuntimeModule () {
            Null = new NullObject ();
            Object = new PrototypeObject (Null, "null");

            Integer = CreatePrototype ("integer");
            Real = CreatePrototype ("real");
            Boolean = CreatePrototype ("boolean");
            String = CreatePrototype ("string");
            DateTime = CreatePrototype ("date-time");
            Operation = CreatePrototype ("operation");
            Closure = CreatePrototype ("closure");
            Module = CreatePrototype ("module");
            ModuleFrame = CreatePrototype ("module-frame");
            Dictionary = CreatePrototype ("dictionary");
            StackFrame = CreatePrototype ("stack-frame");
            DynamicStackFrame = CreatePrototype ("dynamic-stack-frame");
            List = CreatePrototype ("list");
            Set = CreatePrototype ("set");
            Structure = CreatePrototype ("structure");
            Symbol = CreatePrototype ("symbol");
            Constructor = CreatePrototype ("constructor");
        }

        public static Func<object, AnyObject> ConvertToRuntime;

        public static readonly AnyObject Integer;
        public static readonly AnyObject Real;
        public static readonly AnyObject Boolean;
        public static readonly AnyObject String;
        public static readonly AnyObject DateTime;
        public static readonly AnyObject Operation;
        public static readonly AnyObject Closure;
        public static readonly AnyObject Module;
        public static readonly AnyObject ModuleFrame;
        public static readonly AnyObject Dictionary;
        public static readonly AnyObject StackFrame;
        public static readonly AnyObject DynamicStackFrame;
        public static readonly AnyObject List;
        public static readonly AnyObject Set;
        public static readonly AnyObject Structure;
        public static readonly AnyObject Object;
        public static readonly AnyObject Symbol;
        public static readonly AnyObject Constructor;
        public static readonly AnyObject Null;

        public static AnyObject CreateInteger (int n) {
            return new IntegerObject (Integer, n);
        }

        public static AnyObject CreateString (string s) {
            return new StringObject (String, s);
        }

        public static AnyObject CreateBoolean (bool b) {
            return new BooleanObject (Boolean, b);
        }

        public static AnyObject CreateReal (double d) {
            return new RealObject (Real, d);
        }

        public static AnyObject CreateDateTime (DateTime d) {
            return new DateTimeObject (DateTime, d);
        }

        public static AnyObject CreateStructure (params AnyObject [] fields) {
            return new StructureObject (Structure, fields);
        }

        public static AnyObject CreateSet (IEnumerable<AnyObject> items) {
            return new SetObject (Set, items);
        }

        public static AnyObject CreateSet (params AnyObject [] items) {
            return new SetObject (Set, items);
        }

        public static AnyObject CreateList (IEnumerable<AnyObject> items) {
            return new ListObject (List, items);
        }

        public static AnyObject CreateList (params AnyObject [] items) {
            return new ListObject (List, items);
        }

        public static AnyObject CreateDictionary (params AnyObject [] entries) {
            return new DictionaryObject (Dictionary, entries);
        }

        public static AnyObject CreateModule (Namespace ns) {
            return new ModuleObject (Module, ns);
        }

        public static AnyObject CreatePrototype () {
            return new PrototypeObject (Object);
        }

        private static AnyObject CreatePrototype (string name) {
            return new PrototypeObject (Object, name);
        }

        public static AnyObject CreatePrototype (AnyObject prototype) {
            return new PrototypeObject (prototype);
        }

        public static AnyObject CreateNative<T> (T native) {
            return new NativeObject<T> (native);
        }

        public static AnyObject CreateProtocol () {
            return new ProtocolObject ();
        }

        public static AnyObject ObjectFromNative (object nativeObject) {
            return ConvertToRuntime (nativeObject);
        }
    }
}
