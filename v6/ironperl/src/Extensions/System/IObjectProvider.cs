using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    /// It is an ultimate class for all object factories
    /// </summary>
    public interface IObjectFactory
    {
        /// <summary>
        /// Create object with parameters, which is specified by the current factory
        /// </summary>
        /// <returns></returns>
        object CreateObject();

        /// <summary>
        /// Load object into factory
        /// </summary>
        /// <param name="obj">Object to be loaded</param>
        /// <exception cref="System.NotSupportedException">Object factory can be used only for emitting objects</exception>
        void LoadState(object obj);
    }
}
