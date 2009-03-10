using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Runtime {
    public class ParametersSchemaObject : SchemaObject {
        int RequiredParameters;
        AnyObject [] Parameters;
        MatchingSchemaObject RemainingParameters;

        public ParametersSchemaObject (params AnyObject [] parameters) : this (false, parameters) { }

        public ParametersSchemaObject (bool listParameters, params AnyObject [] parameters) {
            if (listParameters) {
                RemainingParameters = parameters [parameters.Length - 1].Expect<MatchingSchemaObject> ();
                Parameters = new AnyObject [parameters.Length - 1];
                Array.Copy (parameters, Parameters, parameters.Length - 1);
            } else {
                Parameters = parameters;
            }

            RequiredParameters = Parameters.Length;
        }

        public ParametersSchemaObject (int requiredParameters, params AnyObject [] parameters) {
            RequiredParameters = requiredParameters;
            Parameters = parameters;
        }

        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            if (RemainingParameters != null && arguments.Length >= RequiredParameters && arguments.Length >= Parameters.Length) {
                int n;
                for (n = 0; n < Parameters.Length; n++) {
                    if (!Parameters [n].Match (results, arguments [n])) {
                        return false;
                    }
                }

                AnyObject rest = null;
                if (!results.IsNull) {
                    rest = RuntimeModule.CreateList ();
                    results.SetVariable (results, RemainingParameters.Name, RemainingParameters.FrameIndex, rest);
                }

                for (; n < arguments.Length; n++) {
                    if (!RemainingParameters.Match (RuntimeModule.Null, arguments [n])) {
                        return false;
                    }

                    if (rest != null) rest.Add (arguments [n]);
                }

                return true;
            } else if (arguments.Length >= RequiredParameters && arguments.Length <= Parameters.Length) {
                for (int n = 0; n < arguments.Length; n++) {
                    if (!Parameters [n].Match (results, arguments [n])) {
                        return false;
                    }
                }

                return true;
            } else {
                return false;
            }
        }

        public override IEnumerable<AnyObject> ObjectReferences {
            get {
                return Parameters;
            }
        }
    }
}
