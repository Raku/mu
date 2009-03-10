using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.Reflection.Extensions;
using System.Extensions;

namespace System.Threading
{
    /// <summary>
    /// Represents all required information for
    /// asycnrhonous invocation
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class AsyncMethod<T>: IDisposable
    {
        private readonly MethodBase m_asyncMethod;
        private readonly Dictionary<string, object> m_arguments;
        private readonly WaitCallback m_thread;
        private readonly ManualResetEvent m_resetEvent;
        private bool m_disposed;
        private T m_result;
        private object m_owner;

        private AsyncMethod()
        {
            m_result = default(T);
            m_disposed = false;
            m_arguments = new Dictionary<string, object>();
            m_resetEvent = new ManualResetEvent(false);
            m_thread = (state) =>
                {
                    m_result = m_asyncMethod.Invoke<T>(m_owner, m_arguments.Values.ToArray());
                    m_resetEvent.Set();
                    Invoked = false;
                };
        }

        public AsyncMethod(Type target, string methodName, Type returnType, params Type[] parameters)
            :this()
        {
            ExceptionManager.CheckOnNull(target, "target");
            ExceptionManager.CheckOnNull(methodName, "methodName");
            if (returnType.IsNull()) returnType = typeof(void);
            m_asyncMethod = target.GetMethod(methodName,
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
                returnType,
                parameters);
            //Throw exception if target method is not found
            if (m_asyncMethod.IsNull())
                ExceptionManager.Throw<MissingMethodException>(target.FullName, methodName);
        }

        public AsyncMethod(object owner, string methodName)
            : this(owner, methodName, 0)
        {
        }

        public AsyncMethod(object owner, string methodName, byte overloadIndex)
            :this()
        {
            ExceptionManager.CheckOnNull(owner, "owner");
            ExceptionManager.CheckOnNull(methodName, "methodName");
            var methods = owner.GetType().GetOverloads(methodName, BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
            if (methods.Length < overloadIndex)
                throw new ArgumentOutOfRangeException("overloadIndex");
            m_asyncMethod = methods[overloadIndex];
            m_owner = owner;
        }

        /// <summary>
        /// Get or set method owner
        /// </summary>
        public object Owner
        {
            get
            {
                if (Invoked)
                    ExceptionManager.Throw<InvalidOperationException>();
                return m_owner;
            }
            set
            {
                if (Invoked)
                    ExceptionManager.Throw<InvalidOperationException>();
                m_owner = value;
            }
        }

        /// <summary>
        /// Get or set parameter value
        /// </summary>
        /// <param name="paramName">Name of the parameter</param>
        /// <returns>Value of the parameter</returns>
        public object this[string paramName]
        {
            get
            {
                ExceptionManager.CheckOnNull(paramName, "paramName");
                if (!m_arguments.ContainsKey(paramName) || Invoked)
                    ExceptionManager.Throw<InvalidOperationException>();
                return m_arguments[paramName];
            }
            set
            {
                var parameter = m_asyncMethod.GetParameter(paramName);
                if (paramName.IsNull() || Invoked)
                    ExceptionManager.Throw<InvalidOperationException>();
                m_arguments[paramName] = value;
            }
        }

        /// <summary>
        /// Get value indicating that the current method is invoked
        /// </summary>
        public bool Invoked
        {
            get;
            private set;
        }

        /// <summary>
        /// Executes the current method in separated thread
        /// </summary>
        public void InvokeAsync()
        {
            Invoked = true;
            ThreadPool.QueueUserWorkItem(m_thread);
        }

        /// <summary>
        /// Executes the current method in separated thread
        /// </summary>
        /// <param name="args">Method arguments</param>
        public void InvokeAsync(params object[] args)
        {
            ExceptionManager.CheckOnNull(args, "args");
            var parameters = m_asyncMethod.GetParameters();
            for (var i = 0; i < Math.Min(parameters.Length, args.Length); i++)
                this[parameters[i].Name] = args[i];
            InvokeAsync();
        }

        /// <summary>
        /// Get result of asynchronous invocation
        /// </summary>
        public T Result
        {
            get
            {
                m_resetEvent.WaitOne();
                return m_result;
            }
        }

        #region IDisposable Members

        /// <summary>
        /// Release all runtime information associated
        /// with asynchronous invocation
        /// </summary>
        public void Dispose()
        {
            if (!m_disposed)
                m_resetEvent.Close();
            m_disposed = true;
        }

        #endregion

        ~AsyncMethod()
        {
            Dispose();
        }
    }
}
