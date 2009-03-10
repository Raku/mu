using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Utilities;

namespace Tycho.Runtime {
    public class MatchingSchemaObject : SchemaObject {
        public AnyObject Name { get; private set; }
        public int FrameIndex { get; private set; }
        public AnyObject Schema { get; private set; }

        public MatchingSchemaObject (AnyObject name, int frameIndex) : this (name, frameIndex, RuntimeModule.Null) { }

        public MatchingSchemaObject (AnyObject name, int frameIndex, AnyObject schema) {
            Name = name;
            FrameIndex = frameIndex;
            Schema = schema;
        }

        public override bool Match (AnyObject results, params AnyObject [] arguments) {
            if (Schema.IsNull || Schema.Match (results, arguments)) {
                if (!results.IsNull) {
                    results.SetVariable (results, Name, FrameIndex, arguments [0]);
                }

                return true;
            } else {
                return false;
            }
        }

        [TychoFunctionSchema ("matching-schema")]
        static AnyObject MatchingSchemaObjectSchema = new ParametersSchemaObject (new AnySchemaObject (), new AnySchemaObject (), new AnySchemaObject ());
        [TychoFunction ("matching-schema")]
        static AnyObject CreateNew (params AnyObject [] arguments) {
            return new MatchingSchemaObject (arguments [0], arguments [1].ExpectValue<int> (), arguments [2]);
        }

        public override string ToString (HashSet<AnyObject> done) {
            if (!Schema.IsNull) {
                return Name.ToString (done) + " :: " + Schema.ToString (done);
            } else {
                return Name.ToString (done);
            }
        }
    }
}
