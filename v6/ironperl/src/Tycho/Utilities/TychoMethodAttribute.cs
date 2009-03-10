using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;

namespace Tycho.Utilities {
    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoMethodAttribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoMethodAttribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoMethod2Attribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoMethod2Attribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoGetterAttribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoGetterAttribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoSetterAttribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoSetterAttribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoGetter2Attribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoGetter2Attribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoSetter2Attribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoSetter2Attribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = true)]
    public class TychoMethodSchemaAttribute : Attribute {
        public string Name { get; set; }
        public string Object { get; set; }
        public string SchemaName { get; set; }

        public TychoMethodSchemaAttribute (string obj, string name) {
            Object = obj;
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoFunctionAttribute : Attribute {
        public string Name { get; set; }

        public TychoFunctionAttribute (string name) {
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method, AllowMultiple = true)]
    public class TychoFunction2Attribute : Attribute {
        public string Name { get; set; }

        public TychoFunction2Attribute (string name) {
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = true)]
    public class TychoFunctionSchemaAttribute : Attribute {
        public string Name { get; set; }

        public TychoFunctionSchemaAttribute (string name) {
            Name = name;
        }
    }

    [AttributeUsage (AttributeTargets.Method)]
    public class TychoModuleLoadAttribute : Attribute { }

    [AttributeUsage (AttributeTargets.Field | AttributeTargets.Property)]
    public class TychoObjectAttribute : Attribute {
        public string Name { get; set; }

        public TychoObjectAttribute (string name) {
            Name = name;
        }
    }
}
