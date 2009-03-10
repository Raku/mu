using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using Tycho.Lexer;
using Tycho.Parser.Tokens;
using Tycho.Parser;
using Tycho.Compiler;
using System.Reflection;
using System.IO;

namespace IronPerl {
    public class ExpressionLanguage {
        IParser ExpressionParser;
        ExpressionCompiler ExpressionCompiler;
        
        public ExpressionLanguage (AnyObject macroContext, Namespace defaultNamespace) {
            ExpressionParser = GetExpressionParser (macroContext);
            ExpressionCompiler = new ExpressionCompiler (defaultNamespace);
        }

        IParser GetExpressionParser (AnyObject macroContext) {
            ParserLanguage plang = new ParserLanguage ();
            plang.MacroBuilder = new MacroBuilder (macroContext);
            plang.TokensParser = new TokenListParser ();
            plang.MacroParser = GetParser ("Tycho.Language.macro-expression.language", "expression", new ParserLanguage ());
            return GetParser ("Tycho.Language.expression.language", "expression", plang);
        }

        IParser GetParser (string resource, string parser, ParserLanguage plang) {
            Assembly assembly = typeof (ExpressionLanguage).Assembly;
            StreamReader file = new StreamReader (assembly.GetManifestResourceStream (resource));
            return plang.BuildParsers (file.ReadToEnd ()) [parser];
        }
        
        public AnyObject LoadModule (string expression, string filename, Namespace moduleNamespace, AnyObject context, IModuleScopeLoader moduleLoader) {
            FrameSymbolScope moduleScope = new FrameSymbolScope (new ModuleSymbolScope (moduleLoader));
            FrameSymbolScope scope = new FrameSymbolScope (moduleScope);
            AnyObject moduleFrame = new StackFrameObject (RuntimeModule.StackFrame, context);
            AnyObject stackFrame = new StackFrameObject (RuntimeModule.StackFrame, moduleFrame);

            AnyObject moduleInit = CompileOperation (expression, filename, scope, moduleScope, stackFrame, true);
            moduleInit.Invoke ();

            AnyObject module = RuntimeModule.CreateModule (moduleNamespace);

            foreach (AnyObject varname in moduleFrame.Variables) {
                module.SetVariable (module, varname, 0, moduleFrame.GetVariable (moduleFrame, varname, 0));
            }

            return module;
        }

        public AnyObject CompileOperation (string expression, string filename, AnyObject context, IModuleScopeLoader moduleLoader) {
            FrameSymbolScope scope = BuildScope (context, new ModuleSymbolScope (moduleLoader));
            return CompileOperation (expression, filename, scope, null, context, false);
        }

        private AnyObject CompileOperation (string expression, string filename, FrameSymbolScope scope, FrameSymbolScope moduleScope, AnyObject context, bool atModuleLevel) {
            ExpressionCompiler.ModuleScope = moduleScope;

            AST ast = Parser.Parse(expression);
            
            AnyObject assembly = ExpressionCompiler.Compile (ast, scope, atModuleLevel);

            if (ExpressionCompiler.Errors.Count > 0) {
                throw ExpressionCompiler.Errors [0];
            }

            SourceLocation sloc = new SourceLocation ("foo", "foo", 1, 1, 1, 1);

            ByteCodeCompiler bc = new ByteCodeCompiler ();
            ClosureObject closure = bc.GenerateClosure (assembly, context, sloc);

            closure.IsTopLevel = true;
            return closure;
        }
    
        public static FrameSymbolScope BuildScopeRecursive (AnyObject frame) {
            FrameSymbolScope outerScope = null;
            AnyObject outerScopeObject = frame.OuterScope;

            if (!outerScopeObject.IsNull) {
                outerScope = BuildScopeRecursive (outerScopeObject);
            }

            return BuildScope (frame, outerScope);
        }

        private static FrameSymbolScope BuildScope (AnyObject frame, SymbolScope outerScope) {
            FrameSymbolScope scope = new FrameSymbolScope (outerScope);
            
            foreach (AnyObject variable in frame.Variables) {
                scope.DeclareVariableOverride (variable, null);
            }

            return scope;
        }
    }
}
