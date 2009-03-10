using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.Compatibility
{
    /// <summary>
    /// Provides a way to describe object functionality.
    /// </summary>
    /// <typeparam name="T">Type of the functionality description.</typeparam>
    public interface IFunctionalityDescriptor<T>
        where T : IObjectFunctionality
    {
        /// <summary>
        /// Get functionality description.
        /// </summary>
        T Functionality { get; }
    }

    
}
