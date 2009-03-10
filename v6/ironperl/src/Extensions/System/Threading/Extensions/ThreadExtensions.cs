using System.Extensions;

namespace System.Threading.Extensions
{
    /// <summary>
    /// Represents an extension methods for the System.Threading.Thread class
    /// </summary>
    [TypeExtender(typeof(Thread))]
    public static class ThreadExtensions
    {
        /// <summary>
        /// Compare two threads
        /// </summary>
        /// <param name="thread">Target thread</param>
        /// <param name="other">A thread to compare with this thread</param>
        /// <returns>true if the current thread is the same thread; otherwise, false.</returns>
        public static bool Equals(this Thread thread, Thread other)
        {
            ExceptionManager.CheckOnNull(thread, "thread");
            if (other.IsNull()) return false;
            return other.ManagedThreadId == thread.ManagedThreadId;
        }
    }
}
