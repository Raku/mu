using System;
using System.Collections.Generic;
using Tycho.Parser.Tokens;

namespace Tycho.Runtime {
    public class DefaultRuntimeModule : IRuntimeModule {
        public DefaultRuntimeModule () {
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

        public AnyObject Null { get; set; }
        public AnyObject Object { get; set; }

        public AnyObject Integer { get; set; }
        public AnyObject Real { get; set; }
        public AnyObject Boolean { get; set; }
        public AnyObject String { get; set; }
        public AnyObject DateTime { get; set; }
        public AnyObject Operation { get; set; }
        public AnyObject Closure { get; set; }
        public AnyObject Module { get; set; }
        public AnyObject ModuleFrame { get; set; }
        public AnyObject Dictionary { get; set; }
        public AnyObject StackFrame { get; set; }
        public AnyObject DynamicStackFrame { get; set; }
        public AnyObject List { get; set; }
        public AnyObject Set { get; set; }
        public AnyObject Structure { get; set; }
        public AnyObject Symbol { get; set; }
        public AnyObject Constructor { get; set; }

        public AnyObject CreateInteger (int n) {
            return new IntegerObject (Integer, n);
        }

        public AnyObject CreateReal (double n) {
            return new RealObject (Real, n);
        }

        public AnyObject CreateBoolean (bool b) {
            return new BooleanObject (Boolean, b);
        }

        public AnyObject CreateString (string s) {
            return new StringObject (String, s);
        }

        public AnyObject CreateDateTime (DateTime d) {
            return new DateTimeObject (DateTime, d);
        }

        public AnyObject CreateStructure (params AnyObject[] fields) {
            return new StructureObject (Structure, fields);
        }

        public AnyObject CreateList (IEnumerable<AnyObject> items) {
            return new ListObject (List, items);
        }

        public AnyObject CreateSet (IEnumerable<AnyObject> items) {
            return new SetObject (Set, items);
        }

        public AnyObject CreateDictionary (params AnyObject[] entries) {
            return new DictionaryObject (Dictionary, entries);
        }

        public AnyObject CreateClosure (ByteCode[] code, AnyObject[] constants, AnyObject parametersSchema, AnyObject context, SourceLocation sloc) {
            return new ClosureObject (code, constants, parametersSchema, context, sloc);
        }

        public AnyObject CreateModule (Namespace ns) {
            return new ModuleObject (Module, ns);
        }

        private AnyObject CreatePrototype (string name) {
            return new PrototypeObject (Object, name);
        }

        public AnyObject CreatePrototype () {
            return new PrototypeObject (Object);
        }

        public AnyObject CreatePrototype (AnyObject parent) {
            return new PrototypeObject (parent);
        }

        public AnyObject CreateNative<T> (T native) {
            return new NativeObject<T> (native);
        }

        public AnyObject CreateProtocol () {
            return new ProtocolObject ();
        }
    }
}
