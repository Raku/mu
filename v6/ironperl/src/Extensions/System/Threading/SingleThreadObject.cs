using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;
using System.Threading.Extensions;
using System.Runtime.CompilerServices;
using System.Diagnostics;
using System.Runtime.Serialization;

namespace System.Threading
{
    using BindingFlags = System.Reflection.BindingFlags;

    /// <summary>
    /// Represents object, which cannot be distribute between
    /// threads. 
    /// </summary>
    /// <remarks>Any operations under this object can be
    /// performed only in the creator thread.</remarks>
    [Serializable]
    public abstract class SingleThreadObject: Serializable
    {
        private readonly Thread m_owner;

        /// <summary>
        /// Initialize instance of the SingleThreadObject class
        /// </summary>
        protected SingleThreadObject()
        {
            m_owner = Thread.CurrentThread;
        }

        /// <summary>
        /// Get thread in which the current object is created
        /// </summary>
        protected Thread Owner
        {
            get { return m_owner; }
        }

        /// <summary>
        /// Returns a value indicating that specified thread can
        /// invoke methods from the current object
        /// </summary>
        /// <param name="target">Target thread</param>
        /// <returns>True, if specified thread can invoke methods
        /// from the current object</returns>
        public bool IsAccessibleFrom(Thread target)
        {
            return Owner.Equals(target);
        }

        /// <summary>
        /// Method is called immediately after deserialization of the object.
        /// </summary>
        /// <param name="context">Deserialization context</param>
        protected override void OnAfterDeserialization(StreamingContext context)
        {
            //Initialize current instance and save creator thread
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic;
            var ownerField = this.GetType().GetField("m_owner", flags);
            //Why I use Reflection? Because m_owner field is init-only.
            ownerField.SetValue(this, Thread.CurrentThread);
        }

        /// <summary>
        /// Checks the caller thread. If caller thread 
        /// is not an object owner then method
        /// throws exception
        /// </summary>
        /// <exception cref="System.InvalidOperationException">Caller thread doesn't have
        /// access to this object</exception>
        [MethodImpl(MethodImplOptions.NoInlining)]
        [DebuggerHidden]
        [DebuggerNonUserCode]
        protected void CheckThreadAccess()
        {
            if (!IsAccessibleFrom(Thread.CurrentThread))
                throw new InvalidOperationException();
        }
    }
}
