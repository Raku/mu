using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Parser.Tokens;
using Tycho.Runtime;

namespace Tycho.Parser {
    public interface IParser {
        string Name { get; }
        IParser ExtendParser ();
        AnyObject Parse (List<List<Token>> tokensList);
        AnyObject Parse (List<Token> tokens);
    }
}
