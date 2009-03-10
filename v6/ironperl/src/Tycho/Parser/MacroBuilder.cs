using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Parser.Tokens;
using Tycho.Compiler;
using Tycho.Language;

namespace Tycho.Parser {
    /*public class MacroBuilder {
        AnyObject RuntimeContext;
        AnyObject GeneratedModule;
        ModuleMap ModuleMap;

        public MacroBuilder (AnyObject runtimeContext) {
            RuntimeContext = runtimeContext;
            GeneratedModule = CompilerModule.CreateList (new AnyObject [] { Namespace.NextGeneratedName () });
            ModuleMap = new ModuleMap (Namespaces.Parser);
        }

        TermGenerator UnquoteExpression (AnyObject expression) {
            return new QuoteVariableGenerator (ModuleMap.ExpectSymbol (expression));
        }

        public AnyObject BuildMacro (IEnumerable<AnyObject> productionNames, SourceLocation sloc, AnyObject macroBody, bool isQuote) {
            if (isQuote) {
                TermGeneratorBuilder builder = new TermGeneratorBuilder (GeneratedModule);
                return new MacroOperation (builder.Build (macroBody, UnquoteExpression));
            } else {
                FrameSymbolScope scope = new FrameSymbolScope (ExpressionLanguage.BuildScopeRecursive (RuntimeContext));

                AnyObject parameters = CompilerModule.CreateTerm (sloc);
                foreach (AnyObject production in productionNames) {
                    parameters.SetProperty (production, new MatchingSchemaObject (production, 0));
                    scope.DeclareVariable (production, sloc);
                }

                ExpressionCompiler expressionCompiler = new ExpressionCompiler (Namespaces.Parser);
                AnyObject bodyExpression = expressionCompiler.Compile (macroBody, scope, false);

                ByteCodeCompiler byteCodeCompiler = new ByteCodeCompiler ();
                ByteCodeOutput bodyByteCode = byteCodeCompiler.GenerateCode (bodyExpression);

                return new ClosureObject (bodyByteCode.GetCode (), bodyByteCode.GetConstants (), parameters, new ModuleSecurityObject (Namespaces.Parser, RuntimeContext), sloc);
            }
        }
    }

    public class TermGeneratorBuilder {
        public delegate TermGenerator UnquoteExpressionDelegate (AnyObject expression);

        AnyObject GeneratedModule;

        public TermGeneratorBuilder (AnyObject generatedModule) {
            GeneratedModule = generatedModule;
        }

        private bool IsUnquotedEtcSymbol (AnyObject term) {
            if (IsUnquoted (term)) {
                AnyObject expression = term.GetProperty (ExpressionCompiler.ExpressionSymbol);
                if (expression.GetProperty (Symbols.ParserTermName) == ExpressionCompiler.EtcSymbol) {
                    return true;
                }
            }

            return false;
        }

        private bool IsUnquoted (AnyObject term) {
            return term is StructureObject && term.GetProperty (Symbols.ParserTermName) == ExpressionCompiler.UnquoteSymbol;
        }

        private bool ListHasVariableItems (ListObject list, out AnyObject variableItem) {
            List<AnyObject> items = list.Items;

            if (items.Count >= 2) {
                AnyObject secondLast = items [items.Count - 2];
                AnyObject last = items [items.Count - 1];

                if (IsUnquoted (secondLast) && IsUnquotedEtcSymbol (last)) {
                    variableItem = secondLast;
                    return true;
                }
            }

            variableItem = null;
            return false;
        }

        public TermGenerator Build (AnyObject term, UnquoteExpressionDelegate unquoteExpression) {
            if (term is ListObject) {
                AnyObject variableExpression;
                ListGenerator list = new ListGenerator ();

                ListObject listTerm = (ListObject) term;
                if (ListHasVariableItems (listTerm, out variableExpression)) {
                    list.VariableItems = unquoteExpression (variableExpression.GetProperty (ExpressionCompiler.ExpressionSymbol));

                    for (int n = 0; n < listTerm.Items.Count - 2; n++) {
                        list.Items.Add (Build (listTerm.Items [n], unquoteExpression));
                    }
                } else {
                    foreach (AnyObject item in term) {
                        list.Items.Add (Build (item, unquoteExpression));
                    }
                }

                return list;
            } else if (term is StructureObject) {
                if (term.HasProperty (Symbols.ParserTermName)
                    && term.GetProperty (Symbols.ParserTermName) == ExpressionCompiler.UnquoteSymbol) {
                    return unquoteExpression (term.GetProperty (ExpressionCompiler.ExpressionSymbol));
                } else {
                    if (term.HasProperty (Symbols.ParserTermName)
                        && term.GetProperty (Symbols.ParserTermName) == Symbols.ParserIdentifier
                        && !term.HasProperty (Symbols.ParserModule)) {
                        term.SetProperty (Symbols.ParserModule, GeneratedModule);
                    }

                    StructureGenerator structure = new StructureGenerator (term.SourceLocation);

                    foreach (KeyValuePair<AnyObject, AnyObject> field in term.Expect<StructureObject> ().Fields) {
                        structure.Fields.Add (new StructureGenerator.Field () { Name = field.Key, Value = Build (field.Value, unquoteExpression) });
                    }

                    return structure;
                }
            } else {
                return new ConstantGenerator (term);
            }
        }
    }

    public abstract class TermGenerator {
        public abstract AnyObject BuildTerm (params AnyObject [] arguments);
    }

    class QuoteVariableGenerator : TermGenerator {
        AnyObject FieldName;

        public QuoteVariableGenerator (AnyObject fieldName) {
            FieldName = fieldName;
        }

        public override AnyObject BuildTerm (params AnyObject [] arguments) {
            return arguments [0].GetProperty (FieldName);
        }
    }

    class QuoteExpressionGenerator : TermGenerator {
        private int ArgumentIndex;

        public QuoteExpressionGenerator (int argumentIndex) {
            ArgumentIndex = argumentIndex;
        }

        public override AnyObject BuildTerm (params AnyObject[] arguments) {
            return arguments[ArgumentIndex];
        }
    }

    class ConstantGenerator : TermGenerator {
        AnyObject Value;

        public ConstantGenerator (AnyObject value) {
            Value = value;
        }

        public override AnyObject BuildTerm (params AnyObject [] arguments) {
            return Value;
        }
    }

    class ListGenerator : TermGenerator {
        public List<TermGenerator> Items;
        public TermGenerator VariableItems;

        public ListGenerator () {
            Items = new List<TermGenerator> ();
        }

        public override AnyObject BuildTerm (params AnyObject [] arguments) {
            AnyObject list = CompilerModule.CreateTermList ();

            foreach (TermGenerator item in Items) {
                list.Add (item.BuildTerm (arguments));
            }

            if (VariableItems != null) {
                AnyObject variableItemsTerm = VariableItems.BuildTerm (arguments);
                foreach (AnyObject varItem in variableItemsTerm) {
                    list.Add (varItem);
                }
            }

            return list;
        }
    }

    class StructureGenerator : TermGenerator {
        public class Field {
            public AnyObject Name;
            public TermGenerator Value;
        }

        public List<Field> Fields;
        SourceLocation SourceLocation;

        public StructureGenerator (SourceLocation sourceLocation) {
            Fields = new List<Field> ();
            SourceLocation = sourceLocation;
        }

        public override AnyObject BuildTerm (params AnyObject [] arguments) {
            AnyObject term = CompilerModule.CreateTerm (SourceLocation);

            foreach (Field field in Fields) {
                term.SetProperty (field.Name, field.Value.BuildTerm (arguments));
            }

            return term;
        }
    }

    class MacroOperation : NativeOperation {
        TermGenerator TermGenerator;

        public MacroOperation (TermGenerator termGenerator)
            : base (new AnySchemaObject ()) {
            TermGenerator = termGenerator;
        }

        public override AnyObject InvokeWithoutSchema (params AnyObject [] arguments) {
            return TermGenerator.BuildTerm (arguments);
        }
    }*/
}
