using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Compiler;
using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Parser {
    public class TokenListParser : IParser {
        #region IParser Members

        public string Name {
            get {
                return "tokens";
            }
        }

        public IParser ExtendParser () {
            return this;
        }

        public AnyObject Parse (List<List<Token>> tokensList) {
            return CompilerModule.CreateTermList (from tokens in tokensList select Parse (tokens));
        }

        public AnyObject Parse (List<Token> tokens) {
            return new NativeObject<List<Token>> (tokens);
        }

        #endregion
    }
}
