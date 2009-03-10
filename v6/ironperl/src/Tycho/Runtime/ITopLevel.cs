using Tycho.Runtime;

namespace Tycho.Runtime {
    public interface ITopLevel {
        AnyObject Evaluate (string source);
        AnyObject this[AnyObject name] { get; set; }
    }
}