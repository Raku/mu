using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;
using IronPerl;

namespace Tycho.Runtime {
    public class Symbol : InstanceObject {
        public String Name { get; private set; }
        public Namespace Namespace { get; private set; }
        public bool Unique { get; private set; }

        public static Symbol Parse (string str) {
            return Parse (str, Namespaces.Root);
        }

        public static Symbol Parse(string str, Namespace defaultNamespace) {
        //    str = str.Replace("::",":");
            string [] sections = str.Split (':');
            string [] namespaceSections = new string[sections.Length - 1];
            Array.Copy (sections, namespaceSections, sections.Length - 1);
            Namespace ns = Tycho.Runtime.Namespace.BuildNamespace (namespaceSections);

            if (ns == Namespaces.Root) {
                ns = defaultNamespace;
            }

            return ns.Get (sections[sections.Length - 1]);
        }

        public Symbol (AnyObject prototype, string name, Namespace namespace_)
            : this (prototype, name, namespace_, false) {
        }

        public Symbol (AnyObject prototype, string name, Namespace namespace_, bool unique) : base (prototype) {
            Name = name;
            Namespace = namespace_;
            Unique = unique;
        }

        public override string ToString (HashSet<AnyObject> done) {
            return Namespace + Name;
        }

        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            return arguments [0] == this;
        }

        public override AnyObject Serialize () {
            return this;
        }

        public override AnyObject ActuallySerialize () {
            return this;
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                return RuntimeModule.CreateSet ();
            }
        }

        [TychoMethodSchema ("symbol", "match")]
        static AnyObject PrototypeMatchSchema = new AnySchemaObject ();
        [TychoMethod ("symbol", "match")]
        static AnyObject PrototypeMatch (AnyObject self, params AnyObject [] arguments) {
            return RuntimeModule.CreateBoolean (self.Match (arguments [0], arguments [1]));
        }
    }

    public class Namespace : SimpleObject {
        Dictionary<String, Symbol> Symbols { get; set; }
        Dictionary<String, Namespace> SubNamespaces { get; set; }
        public string Name { get; private set; }
        public Namespace Parent { get; private set; }
        static int LastGeneratedModuleNumber = 0;

        public Namespace (string name, Namespace parent) {
            Name = name;
            Parent = parent;

            Symbols = new Dictionary<string, Symbol> ();
            SubNamespaces = new Dictionary<string, Namespace> ();
        }

        public bool IsDecendentOf (Namespace ancestor) {
            Namespace ns = this;

            while (ns != null) {
                if (ns == ancestor) {
                    return true;
                }
                ns = ns.Parent;
            }

            return false;
        }

        public Namespace GetNamespace (string name) {
            Namespace ns;
            if (!SubNamespaces.TryGetValue (name, out ns)) {
                ns = new Namespace (name, this);
                SubNamespaces[name] = ns;
            }
            return ns;
        }

        public static Namespace Parse (string path) {
            return BuildNamespace (path.Split (':'));
        }

        public static Namespace BuildNamespace (params string [] path) {
            Namespace ns = Namespaces.Root;

            foreach (string name in path) {
                ns = ns.GetNamespace (name);
            }

            return ns;
        }

        public Symbol Get (string name) {
            Symbol s;
            if (!Symbols.TryGetValue (name, out s)) {
                s = new Symbol (RuntimeModule.Symbol, name, this);
                Symbols[name] = s;
            }
            return s;
        }

        public override string ToString () {
            if (Parent != null) {
                return Parent + Name + ":";
            } else {
                // this is the Root namespace
                return "";
            }
        }

        public Symbol GetUnique (string name) {
            return new Symbol (RuntimeModule.Symbol, name, this, true);
        }

        public static string NextGeneratedName () {
            return "gen-" + ++LastGeneratedModuleNumber;
        }

        public string [] Path {
            get {
                List<string> path = new List<string> ();
                Namespace ns = this;

                while (ns.Parent != null) {
                    path.Insert (0, ns.Name);
                    ns = ns.Parent;
                }

                return path.ToArray ();
            }
        }
    }

    public class Symbols {
        public static Symbol RuntimeGetProperty = Namespaces.Runtime.Get ("get-property"),
            RuntimeSetProperty = Namespaces.Runtime.Get ("set-property"),
            RuntimeHasProperty = Namespaces.Runtime.Get ("has-property"),
            RuntimeHasMethod = Namespaces.Runtime.Get ("has-method"),
            RuntimeInvoke = Namespaces.Runtime.Get ("invoke"),
            RuntimeMatch = Namespaces.Runtime.Get ("match"),
            RuntimeMatchAssignment = Namespaces.Runtime.Get ("match-assignment"),
            RuntimeProxyTarget = Namespaces.Runtime.Get ("target"),
            RuntimeGetVariable = Namespaces.Runtime.Get ("get-variable"),
            RuntimeDispose = Namespaces.Runtime.Get ("dispose"),
            RuntimeSetVariable = Namespaces.Runtime.Get ("set-variable"),
            RuntimeIndexGet = Namespaces.Runtime.Get ("index-get"),
            RuntimeIndexSet = Namespaces.Runtime.Get ("index-set"),
            RuntimeCount = Namespaces.Runtime.Get ("count"),
            RuntimeAdd = Namespaces.Runtime.Get ("add"),
            RuntimeInsert = Namespaces.Runtime.Get ("insert"),
            RuntimeRemove = Namespaces.Runtime.Get ("remove"),
            RuntimeRemoveIndex = Namespaces.Runtime.Get ("remove-index"),
            RuntimeDictionary = Namespaces.Runtime.Get ("dictionary"),
            RuntimeContainsKey = Namespaces.Runtime.Get ("contains-key"),
            RuntimeContainsValue = Namespaces.Runtime.Get ("contains-value"),
            RuntimeMap = Namespaces.Runtime.Get ("map"),
            RuntimeEach = Namespaces.Runtime.Get ("each"),
            RuntimeFilter = Namespaces.Runtime.Get ("filter"),
            RuntimePrototype = Namespaces.Runtime.Get ("prototype"),
            RuntimeProtectedPrototype = Namespaces.Runtime.Get ("protected-prototype"),
            RuntimePlus = Namespaces.Runtime.Get ("plus"),
            RuntimeMinus = Namespaces.Runtime.Get ("minus"),
            RuntimeMultiply = Namespaces.Runtime.Get ("multiply"),
            RuntimeDivide = Namespaces.Runtime.Get ("divide"),
            RuntimeToPower = Namespaces.Runtime.Get ("to-power"),
            RuntimeLessThan = Namespaces.Runtime.Get ("less-than"),
            RuntimeLessThanEqualTo = Namespaces.Runtime.Get ("less-than-equal-to"),
            RuntimeGreaterThan = Namespaces.Runtime.Get ("greater-than"),
            RuntimeGreaterThanEqualTo = Namespaces.Runtime.Get ("greater-than-equal-to"),
            RuntimeEquals = Namespaces.Runtime.Get ("equals"),
            RuntimeIf = Namespaces.Runtime.Get ("if"),
            RuntimeWhile = Namespaces.Runtime.Get ("while"),
            RuntimePrint = Namespaces.Runtime.Get ("print"),
            RuntimePrintStream = Namespaces.Runtime.Get ("print-stream"),
            RuntimeMoveNext = Namespaces.Runtime.Get ("move-next"),
            RuntimeCurrent = Namespaces.Runtime.Get ("current"),
            RuntimeReset = Namespaces.Runtime.Get ("reset"),
            RuntimeEnumerator = Namespaces.Runtime.Get ("enumerator"),
            RuntimeObject = Namespaces.Runtime.Get ("object"),
            RuntimeClosure = Namespaces.Runtime.Get ("closure"),
            RuntimeStructure = Namespaces.Runtime.Get ("structure"),
            RuntimeList = Namespaces.Runtime.Get ("list"),
            RuntimeSet = Namespaces.Runtime.Get ("set"),
            RuntimeNew = Namespaces.Runtime.Get ("new"),
            RuntimeSpecialize = Namespaces.Runtime.Get ("specialize"),
            RuntimeNewParametersSchema = Namespaces.Runtime.Get ("new-parameters-schema"),
            RuntimeCode = Namespaces.Runtime.Get ("code"),
            RuntimeConstants = Namespaces.Runtime.Get ("constants"),
            RuntimeNull = Namespaces.Runtime.Get ("null"),
            RuntimeMatchingSchema = Namespaces.Runtime.Get ("matching-schema"),
            RuntimeTrue = Namespaces.Runtime.Get ("true"),
            RuntimeFalse = Namespaces.Runtime.Get ("false"),
            RuntimeExit = Namespaces.Runtime.Get ("exit"),
            RuntimeSleep = Namespaces.Runtime.Get ("sleep"),
            RuntimeProperties = Namespaces.Runtime.Get ("properties"),
            RuntimeMethods = Namespaces.Runtime.Get ("methods"),
            RuntimeIsTransactional = Namespaces.Runtime.Get ("is-transactional"),
            RuntimeTransactionalise = Namespaces.Runtime.Get ("transactionalise"),
            RuntimeObjectReferences = Namespaces.Runtime.Get ("object-references"),
            RuntimeInteger = Namespaces.Runtime.Get ("integer"),
            RuntimeBoolean = Namespaces.Runtime.Get ("boolean"),
            RuntimeReal = Namespaces.Runtime.Get ("real"),
            RuntimeString = Namespaces.Runtime.Get ("string"),
            RuntimeDateTime = Namespaces.Runtime.Get ("date-time"),
            RuntimeSymbol = Namespaces.Runtime.Get ("symbol"),
            RuntimeContains = Namespaces.Runtime.Get ("contains"),
            RuntimeSerialize = Namespaces.Runtime.Get ("serialize"),
            RuntimeVariables = Namespaces.Runtime.Get ("variables"),
            RuntimeNamespace = Namespaces.Runtime.Get ("namespace"),
            RuntimeKey = Namespaces.Runtime.Get ("key"),
            RuntimeValue = Namespaces.Runtime.Get ("value"),
            RuntimeAddModule = Namespaces.Runtime.Get ("add-module"),
            RuntimeModule = Namespaces.Runtime.Get ("module"),
            RuntimeStackFrame = Namespaces.Runtime.Get ("stack-frame"),
            RuntimeDynamicStackFrame = Namespaces.Runtime.Get ("dynamic-stack-frame"),
            RuntimeOuterScope = Namespaces.Runtime.Get ("outer-scope"),
            RuntimeModuleFrame = Namespaces.Runtime.Get ("module-frame"),
            RuntimeAddPropertySetter = Namespaces.Runtime.Get ("add-property-setter"),
            RuntimeAddPropertyGetter = Namespaces.Runtime.Get ("add-property-getter"),
            RuntimeAddMethod = Namespaces.Runtime.Get ("add-method"),
            RuntimeAddField = Namespaces.Runtime.Get ("add-field"),
            RuntimeMethod = Namespaces.Runtime.Get ("method"),
            RuntimeField = Namespaces.Runtime.Get ("field"),
            RuntimeGetter = Namespaces.Runtime.Get ("getter"),
            RuntimeSetter = Namespaces.Runtime.Get ("setter"),
            RuntimeName = Namespaces.Runtime.Get ("name"),
            RuntimeCompareTo = Namespaces.Runtime.Get ("compare-to"),
            RuntimeSwitch = Namespaces.Runtime.Get ("switch"),
            RuntimeOperation = Namespaces.Runtime.Get ("operation"),
            RuntimeParametersSchema = Namespaces.Runtime.Get ("invocation-schema"),
            RuntimeClearCache = Namespaces.Runtime.Get ("clear-cache"),
            RuntimeConstructor = Namespaces.Runtime.Get ("constructor"),
            RuntimeConcatenateToString = Namespaces.Runtime.Get ("concatenate-to-string"),
            RuntimeProtocolsImplemented = Namespaces.Runtime.Get ("protocols-implemented"),
            RuntimeAddProtocols = Namespaces.Runtime.Get ("add-protocols"),
            RuntimeConstruct = Namespaces.Runtime.Get ("construct"),
            RuntimeToString = Namespaces.Runtime.Get ("to-string");

        public static Symbol ParserIdentifier = Namespaces.Parser.Get ("identifier"),
            ParserTermName = Namespaces.Parser.Get ("term-name"),
            ParserInteger = Namespaces.Parser.Get ("integer"),
            ParserString = Namespaces.Parser.Get ("string"),
            ParserReal = Namespaces.Parser.Get ("real"),
            ParserError = Namespaces.Parser.Get ("error"),
            ParserSourceLocation = Namespaces.Parser.Get ("source-location"),
            ParserSourceCode = Namespaces.Parser.Get ("source-code"),
            ParserFileName = Namespaces.Parser.Get ("file-name"),
            ParserLineStart = Namespaces.Parser.Get ("line-start"),
            ParserLineEnd = Namespaces.Parser.Get ("line-end"),
            ParserColumnStart = Namespaces.Parser.Get ("column-start"),
            ParserColumnEnd = Namespaces.Parser.Get ("column-end"),
            ParserModule = Namespaces.Parser.Get ("module"),
            ParserGeneratedModule = Namespaces.Parser.Get ("generated-module"),
            ParserSymbol = Namespaces.Parser.Get ("symbol"),
            ParserValues = Namespaces.Parser.Get ("values"),
            ParserInterpolatedString = Namespaces.Parser.Get ("interpolated-string"),
            ParserUnquote = Namespaces.Parser.Get ("unquote");

        public static Symbol SerializationList = Namespaces.Serialization.Get ("list");
    }
}
