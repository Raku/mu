using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Parser.Tokens;
using Tycho.Runtime;
using IronPerl;

namespace Tycho.Compiler {
    public static class CompilerUtilities {
        public static AnyObject SymbolToIdentifier (Symbol symbol, SourceLocation sloc) {
            AnyObject id = CompilerModule.CreateTerm (sloc);
            id.SetProperty (Symbols.ParserTermName, Symbols.ParserIdentifier);
            id.SetProperty (Symbols.ParserIdentifier, RuntimeModule.CreateString (symbol.Name));

            AnyObject module = CompilerModule.CreateTermList ();
            Namespace m = symbol.Namespace;

            while (m != Namespaces.Root) {
                module.Insert (0, RuntimeModule.CreateString (m.Name));
                m = m.Parent;
            }

            id.SetProperty (Symbols.ParserModule, module);

            return id;
        }

        public static AnyObject AssignSourceLocation (AnyObject destinationTerm, AnyObject sourceTerm) {
            if (destinationTerm is StructureObject) {
                destinationTerm.SourceLocation = sourceTerm.SourceLocation;
            }

            return destinationTerm;
        }
    }

    public class ModuleMap {
        public Namespace Default { get; private set; }
        Dictionary<Namespace, Namespace> Map;

        public ModuleMap (ModuleMap parent) {
            Default = parent.Default;
            Map = new Dictionary<Namespace, Namespace> (parent.Map);
        }

        public ModuleMap (Namespace defaultModule) {
            Default = defaultModule;
            Map = new Dictionary<Namespace, Namespace> ();
        }

        public void Add (Namespace alias, Namespace target) {
            Map [alias] = target;
        }

        public Namespace this [Namespace ns] {
            get {
                Namespace result;
                if (Map.TryGetValue (ns, out result)) {
                    return result;
                } else {
                    return ns;
                }
            }
        }

        public Symbol ExpectSymbol (AnyObject term) {
            if (term.HasProperty (Symbols.ParserIdentifier)) {
                Namespace ns;

                if (term.HasProperty (Symbols.ParserModule)) {
                    ns = Namespaces.Root;
                    foreach (AnyObject path in term.GetProperty (Symbols.ParserModule)) {
                        ns = ns.GetNamespace (path.ExpectValue<string> ());
                    }

                    ns = this [ns];
                } else if (term.HasProperty (Symbols.ParserGeneratedModule)) {
                    ns = term.GetProperty (Symbols.ParserGeneratedModule).Expect<Namespace> ();
                } else {
                    ns = Default;
                }

                return ns.Get (term.GetProperty (Symbols.ParserIdentifier).ExpectValue<string> ());
            } else {
                throw new CompilationException ("expected symbol", term);
            }
        }
    }
}
