using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Compiler;
using Tycho.Parser.Tokens;
using Tycho.Runtime;
using IronPerl;

namespace Tycho.Parser {
    public static class Terms {
        public static AnyObject FromReal (double r, SourceLocation sloc) {
            AnyObject str = CompilerModule.CreateTerm (sloc);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserReal);
            str.SetProperty (Symbols.ParserReal, CompilerModule.CreateReal (r));
            return str;
        }

        public static AnyObject FromInteger (int i, SourceLocation sloc) {
            AnyObject str = CompilerModule.CreateTerm (sloc);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserInteger);
            str.SetProperty (Symbols.ParserInteger, CompilerModule.CreateInteger (i));
            return str;
        }

        public static AnyObject FromString (string s, SourceLocation sloc) {
            AnyObject str = CompilerModule.CreateTerm (sloc);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserString);
            str.SetProperty (Symbols.ParserString, CompilerModule.CreateString (s));
            return str;
        }

        public static AnyObject FromIdentifier (string id, List<string> modulePath, SourceLocation sloc) {
            AnyObject str = CompilerModule.CreateTerm (sloc);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserIdentifier);
            str.SetProperty (Symbols.ParserIdentifier, CompilerModule.CreateString (id));

            if (modulePath != null) {
                str.SetProperty (Symbols.ParserModule, CompilerModule.CreateTermList (modulePath.Select ((name) => (AnyObject) CompilerModule.CreateString (name))));
            }

            return str;
        }

        public static AnyObject FromSymbol (Symbol s, SourceLocation sloc) {
            List<string> modulePath = new List<string> ();

            Namespace n = s.Namespace;
            while (n != Namespaces.Root) {
                modulePath.Insert (0, n.Name);
                n = n.Parent;
            }

            return FromIdentifier (s.Name, modulePath, sloc);
        }

        public static AnyObject FromError (string errorMessage, SourceLocation sloc) {
            AnyObject str = CompilerModule.CreateTerm (sloc);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserError);
            str.SetProperty (Symbols.ParserError, RuntimeModule.CreateString (errorMessage));
            return str;
        }

        public static AnyObject FromInterpolatedString (IEnumerable<AnyObject> tokens, SourceLocation sloc) {
            AnyObject str = CompilerModule.CreateTerm (sloc);
            str.SetProperty (Symbols.ParserTermName, Symbols.ParserInterpolatedString);
            str.SetProperty (Symbols.ParserValues, CompilerModule.CreateList (tokens));
            return str;
        }

        public static AnyObject ToTerm (this SourceLocation sloc) {
            AnyObject term = RuntimeModule.CreateStructure ();

            if (sloc.FileName != null) {
                term.SetProperty (Symbols.ParserFileName, CompilerModule.CreateString (sloc.FileName));
            }

            term.SetProperty (Symbols.ParserSourceCode, CompilerModule.CreateString (sloc.Source));

            term.SetProperty (Symbols.ParserLineStart, CompilerModule.CreateInteger (sloc.LineStart));
            term.SetProperty (Symbols.ParserLineEnd, CompilerModule.CreateInteger (sloc.LineEnd));
            term.SetProperty (Symbols.ParserColumnStart, CompilerModule.CreateInteger (sloc.ColumnStart));
            term.SetProperty (Symbols.ParserColumnEnd, CompilerModule.CreateInteger (sloc.ColumnEnd));

            return term;
        }

        public static SourceLocation ToSourceLocation (this AnyObject obj) {
            return new SourceLocation (
                obj.GetProperty (Symbols.ParserSourceCode).ExpectValue<string> (),
                obj.HasProperty (Symbols.ParserFileName)? obj.GetProperty (obj, Symbols.ParserFileName).ExpectValue<string> (): null,
                obj.GetProperty (Symbols.ParserLineStart).ExpectValue<int> (),
                obj.GetProperty (Symbols.ParserLineEnd).ExpectValue<int> (),
                obj.GetProperty (Symbols.ParserColumnStart).ExpectValue<int> (),
                obj.GetProperty (Symbols.ParserColumnEnd).ExpectValue<int> ());
        }
    }
}
