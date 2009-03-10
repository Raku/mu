using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.CompilerServices
{
    /// <summary>
    /// Denotes an unsafe context, which is required for any operation involving pointers
    /// </summary>
    [Serializable]
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Constructor | AttributeTargets.Field, AllowMultiple = false,
        Inherited = false)]
    public sealed class UnsafeAttribute : Attribute
    {
    }
}
