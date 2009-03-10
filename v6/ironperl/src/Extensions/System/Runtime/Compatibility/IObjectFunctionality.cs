using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Compatibility
{
    /// <summary>
    /// Represents object, which describes an object functionality.
    /// </summary>
    /// <remarks>Note for implementers: set accessor for the Item property
    /// must always throws a System.NotSupportedException</remarks>
    public interface IObjectFunctionality: IObjectWithProperties<object>
    {
        /// <summary>
        /// Get functionality description by it name.
        /// </summary>
        /// <param name="functionName">Name of the functionality.</param>
        /// <returns>Descrption functionality.</returns>
        new object this[string functionName] { get; }
    }
}
