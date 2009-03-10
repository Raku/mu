using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Runtime.Serialization;
using System.Runtime.CompilerServices;
using System.Extensions;

namespace System.Threading
{
    using IsolationLevel = System.Data.IsolationLevel;

    /// <summary>
    /// Represents a thread-safe and transaction-safe wrapper for 
    /// another thread-unsafe value
    /// </summary>
    [Serializable]
    public sealed class ThreadShare<T> : Serializable, IThreadSafeObject
    {
        private T m_value;
        private ManualResetEvent m_resetEvent;
        private Thread m_ownerThread;
        private readonly object m_syncRoot;

        public ThreadShare(T value)
        {
            m_value = value;
            m_resetEvent = new ManualResetEvent(false);
            m_syncRoot = new object();
        }

        public ThreadShare()
            : this(default(T))
        {
        }

        public T Value
        {
            get { return m_value; }
            [MethodImpl(MethodImplOptions.Synchronized)]
            set 
            {
                if (m_ownerThread.IsNotNull().AndNot(m_ownerThread.Equals(CurrentThread)))
                    return;
                m_value = value; 
            }
        }

        #region IThreadSafeObject Members

        /// <summary>
        /// Indicates that the current object is in use by something thread
        /// </summary>
        public bool Locked
        {
            get { return m_ownerThread.IsNotNull() ? !m_ownerThread.Equals(CurrentThread) : false; }
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool Lock(bool wait)
        {
            if (!wait && Locked) return false;
            m_resetEvent.WaitOne();
            m_ownerThread = CurrentThread;
            m_resetEvent.Reset();
            return true;
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool Release()
        {
            if (m_ownerThread.IsNull()) return true;
            if (!m_ownerThread.Equals(CurrentThread)) return false;
            m_resetEvent.Set();
            m_ownerThread = CurrentThread;
            return true;
        }

        public object SyncRoot
        {
            get { return m_syncRoot; }
        }

        #endregion

        private static Thread CurrentThread
        {
            get { return Thread.CurrentThread; }
        }
    }
}
