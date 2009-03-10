using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;

namespace System
{
    using StackFrame = System.Diagnostics.StackFrame;
    using MethodBase = System.Reflection.MethodBase;

    /// <summary>
    /// Represents call-stack manager.
    /// </summary>
    public static class StackWalk
    {
        /// <summary>
        /// Get method at the specified position from the top of the call-stack.
        /// </summary>
        /// <param name="index">Method position from the top of the call-stack.</param>
        /// <returns>Method metadata.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static MethodBase GetMethodAtCallStack(int index)
        {
            var frame = new StackFrame(index);
            return frame.GetMethod();
        }

        /// <summary>
        /// Returns a MethodBase object representing the currently executing method.
        /// </summary>
        /// <returns>A MethodBase object representing the currently executing method.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static MethodBase GetCurrentMethod()
        {
            return GetMethodAtCallStack(2);
        }

        /// <summary>
        /// Returns a MethodBase object representing the caller method.
        /// </summary>
        /// <returns>A MethodBase object representing the caller method.</returns>
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static MethodBase GetCallerMethod()
        {
            return GetMethodAtCallStack(3);
        }
    }
}
