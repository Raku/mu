using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.CompilerServices
{
    /// <summary>
    /// Indicates that only one instance of the class is existed at one time
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    [Serializable]
    public sealed class SingletonAttribute : Attribute
    {
    }
}
