using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using Tycho.Runtime;
using IronPerl;

namespace Tycho.Compiler {
    delegate AnyObject TermTransformer<Context> (Context context, TermWrapper term);
    delegate AnyObject PostTransformer (AnyObject resultTerm, AnyObject originalTerm);
    delegate AnyObject PreTransformer (AnyObject term);

    class TermSchemaTransformer<Context> {
        public AnyObject TermName { get; private set; }
        public AnyObject Schema { get; private set; }
        public TermTransformer<Context> Transformer { get; private set; }

        public TermSchemaTransformer (AnyObject termName, AnyObject schema, TermTransformer<Context> transformer) {
            TermName = termName;
            Schema = schema;
            Transformer = transformer;
        }
    }

    class TransformerLookup<Context> {
        Dictionary<AnyObject, List<TermSchemaTransformer<Context>>> Transforms;

        public TransformerLookup () {
            Transforms = new Dictionary<AnyObject, List<TermSchemaTransformer<Context>>> ();
        }

        List<TermSchemaTransformer<Context>> GetTransforms (AnyObject termName) {
            List<TermSchemaTransformer<Context>> transforms;
            if (Transforms.TryGetValue (termName, out transforms)) {
                return transforms;
            } else {
                Transforms.Add (termName, transforms = new List<TermSchemaTransformer<Context>> ());
                return transforms;
            }
        }

        public void Add (TermSchemaTransformer<Context> transformer) {
            GetTransforms (transformer.TermName).Add (transformer);
        }

        public TermSchemaTransformer<Context> FindTransform (AnyObject term) {
            foreach (TermSchemaTransformer<Context> transform in GetTransforms (term.GetProperty (Symbols.ParserTermName))) {
                if ((transform.Schema == null) || transform.Schema.Match (term)) {
                    return transform;
                }
            }

            return null;
        }
    }

    class TermTransform<Context> {
        TransformerLookup<Context> Transformers;
        public TermTransformer<Context> Default { get; set; }
        public PostTransformer PostTransform { get; set; }
        public PreTransformer PreTransform { get; set; }

        public TermTransform () {
            Transformers = new TransformerLookup<Context> ();
        }

        public void AddTransform (string termName, TermTransformer<Context> transformer) {
            Transformers.Add (new TermSchemaTransformer<Context> (Namespaces.Parser.Get (termName), null, transformer));
        }

        public void AddTransform (Symbol termName, TermTransformer<Context> transformer) {
            Transformers.Add (new TermSchemaTransformer<Context> (termName, null, transformer));
        }

        public void AddTransform (StructureObject schema, TermTransformer<Context> transformer) {
            Transformers.Add (new TermSchemaTransformer<Context> (schema.GetProperty (Symbols.ParserTermName), schema, transformer));
        }

        public AnyObject Transform (Context context, AnyObject term) {
            if (PreTransform != null) {
                term = PreTransform (term);
            }

            AnyObject resultTerm = ActuallyTransform (context, term);

            if (PostTransform != null) {
                return PostTransform (resultTerm, term);
            } else {
                return resultTerm;
            }
        }

        private AnyObject ActuallyTransform (Context context, AnyObject term) {
            if (term is ListObject) {
                AnyObject list = CompilerModule.CreateTermList ();

                foreach (AnyObject subTerm in term) {
                    list.Add (Transform (context, subTerm));
                }

                return list;
            } else {
                TermSchemaTransformer<Context> transformer = Transformers.FindTransform (term);
                if (transformer != null) {
                    return transformer.Transformer (context, new TermWrapper (term));
                }

                if (Default != null) {
                    return Default (context, new TermWrapper (term));
                } else {
                    throw CompilationException.TermNotRecognised (term);
                }
            }
        }

        public static AnyObject Schema (string termName, params string [] names) {
            Symbol [] symbols = new Symbol [names.Length];

            for (int n = 0; n < names.Length; n++) {
                symbols [n] = Namespaces.Parser.Get (names [n]);
            }

            return Schema (Namespaces.Parser.Get (termName), symbols);
        }

        public static AnyObject Schema (Symbol termName, params Symbol [] names) {
            AnyObject schema = CompilerModule.CreateStructure ();

            schema.SetProperty (Symbols.ParserTermName, termName);

            foreach (Symbol name in names) {
                schema.SetProperty (name, new MatchingSchemaObject (name, 0));
            }

            return schema;
        }
    }

    class TermWrapper {
        public AnyObject Term { get; private set; }

        public TermWrapper (AnyObject term) {
            Term = term;
        }

        public AnyObject this [string name] {
            get { return Term.GetProperty (Namespaces.Parser.Get (name)); }
        }

        public AnyObject this [Symbol name] {
            get { return Term.GetProperty (name); }
        }

        public bool Has (string name) {
            return Has (Namespaces.Parser.Get (name));
        }

        public bool Has (Symbol symbol) {
            return Term.HasProperty (symbol);
        }
    }
}
