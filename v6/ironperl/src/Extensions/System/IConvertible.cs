using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// Implementation of this interface shows that type supports
    /// conversion to type
    /// </summary>
    /// <typeparam name="T">Type, in which the current object to be converted</typeparam>
    public interface IConvertible<T>
    {
        T Convert();
    }
}
