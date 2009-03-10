using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace System.Extensions
{
    /// <summary>
    /// Represents an exception manager
    /// </summary>
    static class ExceptionManager
    {
        /// <summary>
        /// Tests a specified object on the null reference
        /// </summary>
        /// <param name="value">Object</param>
        /// <param name="paramName">Name of the parameter, which stores specified object</param>
        ///<exception cref="System.ArgumentNullException">value is null.</exception>
        [DebuggerHidden]
        [DebuggerNonUserCode]
        public static void CheckOnNull(object value, string paramName)
        {
            if (value.IsNull())
                Throw<ArgumentNullException>(paramName, System.Properties.Resources.String_ArgumentNull);
        }

        /// <summary>
        /// Throw exception with specified type and arguments
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="args"></param>
        [DebuggerHidden]
        [DebuggerNonUserCode]
        public static void Throw<T>(params object[] args)
            where T : Exception
        {
            var exception = Activator.CreateInstance(typeof(T), args) as Exception;
            throw exception;
        }

        /// <summary>
        /// Throw COM exception for specified HRESULT value
        /// </summary>
        /// <param name="hresult"></param>
        public static void ThrowComException(uint hresult)
        {
            ThrowComException((int)hresult);
        }

        private static void ThrowComException(int hresult)
        {
            if ((hresult & 0x80000000) != 0)
                Marshal.ThrowExceptionForHR(hresult);
        }
    }
}
