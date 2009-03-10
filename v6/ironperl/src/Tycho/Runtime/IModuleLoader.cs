using Tycho.Compiler;
namespace Tycho.Runtime {
    public interface IModuleLoader {
        AnyObject LoadModule (Namespace ns, string [] modulePath, IModuleScopeLoader moduleLoader);
    }

    public class ImmediateModuleLoader : IModuleLoader {
        private AnyObject Module;

        public ImmediateModuleLoader (AnyObject module) {
            Module = module;
        }

        public AnyObject LoadModule (Namespace ns, string [] modulePath, IModuleScopeLoader moduleLoader) {
            return Module;
        }
    }
}