using System.Extensions;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Extensions;

namespace System.Runtime
{
    /// <summary>
    /// Represents host for all projected objects
    /// </summary>
    sealed class ProjectionHost
    {
        private readonly AssemblyBuilder m_projectionHost;
        private static readonly AssemblyName m_hostName;
        private readonly ModuleBuilder m_hostModule;
        private int m_nameCounter;
        private static ProjectionHost m_host;

        static ProjectionHost()
        {
            m_hostName = new AssemblyName("ProjectionCore");
        }

        /// <summary>
        /// Get active instance of the Instance Projection Host
        /// </summary>
        public static ProjectionHost Instance
        {
            get { return m_host.IsNull() ? m_host = new ProjectionHost() : m_host; }
        }

        /// <summary>
        /// Initialize Instance Projection Host (IPH)
        /// </summary>
        public ProjectionHost()
        {
            m_projectionHost = AppDomain.CurrentDomain.DefineDynamicAssembly(m_hostName,
                AssemblyBuilderAccess.Run);
            m_hostModule = m_projectionHost.DefineDynamicModule("projcor");
            m_nameCounter = 0;
        }

        /// <summary>
        /// Create class stub for the specified object
        /// </summary>
        /// <param name="projectedType"></param>
        /// <returns></returns>
        public Type CreateTypeStub(Type projectedType, Type originalType)
        {
            //If stub for specified type is
            var stub = GetTypeStub(projectedType, originalType);
            if (stub.IsNotNull()) return stub;
            var stubBuilder = m_hostModule.DefineType(CreateTypeStubName(projectedType), 
                TypeAttributes.Sealed | TypeAttributes.Public | TypeAttributes.SpecialName | TypeAttributes.AnsiClass);
            //Define initial object storage
            var valueField = stubBuilder.DefineField("m_value", typeof(object), FieldAttributes.Private | FieldAttributes.InitOnly);
            //Define constructor that accepts projected instance
            var ctor = stubBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, new[] { typeof(object) });
            var msilGen = ctor.GetILGenerator();
            //Call base constructor from System.Object
            msilGen.Emit(OpCodes.Ldarg_0);
            msilGen.Emit(OpCodes.Call, typeof(object).GetConstructor(new Type[0]));
            //Now store argument to the field
            msilGen.Emit(OpCodes.Ldarg_0);
            msilGen.Emit(OpCodes.Ldarg_1);
            msilGen.Emit(OpCodes.Stfld, valueField);
            msilGen.Emit(OpCodes.Ret);
            //Inject other interfaces
            InjectAttribute(projectedType, originalType, stubBuilder);
            stubBuilder.AddInterfaceImplementation(projectedType);
            InjectImplementations(projectedType, stubBuilder, valueField);
            InjectMethodStubs(projectedType, stubBuilder, valueField, originalType);
            stub = stubBuilder.CreateType();
            return stub;
        }

        private void InjectMethodStubs(Type projectedType, TypeBuilder stub, FieldInfo valueField, Type originalType)
        {
            foreach (var method in projectedType.GetMethods())
            {
                //Generate method stub
                var methodStub = stub.DefineMethod(String.Format("{0}Stub", method.Name), 
                    MethodAttributes.PrivateScope | MethodAttributes.Final | MethodAttributes.Virtual);
                methodStub.SetParameters(method.GetParameterTypes());
                //Define return type
                methodStub.SetReturnType(method.ReturnType);
                //Represents real method calling
                var wrapCall = originalType.GetMethod(method.Name, method.GetParameterTypes());
                //Create method body
                var msilGen = methodStub.GetILGenerator();
                if (wrapCall.IsNotNull()) 
                    //if original type contains specified public method then 
                    //generate necessary MSIL code
                {
                    //Load object onto stack
                    msilGen.Emit(OpCodes.Ldarg_0);
                    msilGen.Emit(OpCodes.Ldfld, valueField);
                    //Define signature and load arguments onto stack
                    foreach (var parameter in method.GetParameters())
                    {
                        var pos = parameter.Position + 1;
                        methodStub.DefineParameter(pos,
                            parameter.Attributes,
                            parameter.Name);
                        if (parameter.ParameterType.IsByRef)
                            msilGen.Emit(OpCodes.Ldarga, pos);
                        else
                            msilGen.Emit(OpCodes.Ldarg, pos);
                    }
                    msilGen.EmitCall(OpCodes.Call, wrapCall, null);
                    msilGen.Emit(OpCodes.Ret);
                }
                else
                {
                    //Else throw NotImplementedException
                    msilGen.Emit(OpCodes.Newobj,
                        typeof(NotImplementedException).GetConstructor(new Type[0]));  //call instance [mscorlib]System.NotImplementedException::.ctor()
                    msilGen.Emit(OpCodes.Throw);                                       //throw
                }
                stub.DefineMethodOverride(methodStub, method);
            }
        }

        private void InjectImplementations(Type projectedType, TypeBuilder stub, FieldInfo valueField)
        {
            //Iterate through all interfaces and implement each method
            foreach (var iface in projectedType.GetInterfaces())
                foreach (var method in iface.GetMethods())
                {
                    //Generate method stub
                    var methodImpl = stub.DefineMethod(String.Format("{0}.{1}", iface.FullName, method.Name),
                        MethodAttributes.PrivateScope | MethodAttributes.Virtual | MethodAttributes.Final);
                    methodImpl.SetParameters(method.GetParameterTypes());
                    //Define return type
                    methodImpl.SetReturnType(method.ReturnType);
                    //Create method body
                    var msilGen = methodImpl.GetILGenerator();
                    //Load object onto stack
                    msilGen.Emit(OpCodes.Ldarg_0);
                    msilGen.Emit(OpCodes.Ldfld, valueField);
                    //Define signature and load arguments onto stack
                    foreach (var parameter in method.GetParameters())
                    {
                        var pos = parameter.Position + 1;
                        methodImpl.DefineParameter(pos,
                            parameter.Attributes,
                            parameter.Name);
                        if (parameter.ParameterType.IsByRef)
                            msilGen.Emit(OpCodes.Ldarga, pos);
                        else
                            msilGen.Emit(OpCodes.Ldarg, pos);
                    }
                    msilGen.EmitCall(OpCodes.Call, method, null);
                    msilGen.Emit(OpCodes.Ret);
                    stub.DefineMethodOverride(methodImpl, method);
                }
        }

        private void InjectAttribute(Type projectedType, Type originalType, TypeBuilder stub)
        {
            var ctorArg = typeof(ProjectedTypeAttribute).GetConstructor(new[] { typeof(Type), typeof(Type) });
            var attrBuilder = new CustomAttributeBuilder(
                ctorArg,
                new[] { projectedType, originalType });
            stub.SetCustomAttribute(attrBuilder);
        }

        private string CreateTypeStubName(Type projectedType)
        {
            return String.Format("{0}Stub{1}", projectedType.Name, m_nameCounter++);
        }

        private Type GetTypeStub(Type projectedType, Type originalType)
        {
            foreach (var type in m_hostModule.GetTypes())
            {
                var attr = type.GetCustomAttribute<ProjectedTypeAttribute>();
                if (attr.IsNull()) continue;
                if (attr.ProjectedType.Equals(projectedType) &&
                    attr.TargetType.Equals(originalType)) return type;
            }
            return null;
        }
    }
}
