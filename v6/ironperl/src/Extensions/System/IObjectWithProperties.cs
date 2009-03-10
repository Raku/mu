using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    using INotifyPropertyChanged = System.ComponentModel.INotifyPropertyChanged;
    using INotifyPropertyChanging = System.ComponentModel.INotifyPropertyChanging;

    /// <summary>
    /// Represents object with named properties.
    /// </summary>
    public interface IObjectWithProperties<T>: INotifyPropertyChanged, INotifyPropertyChanging
    {
        /// <summary>
        /// Get object properties.
        /// </summary>
        string[] Properties { get; }

        /// <summary>
        /// Get or set value of the object property by it name.
        /// </summary>
        /// <param name="propertyName">Name of the property.</param>
        /// <returns>Property value.</returns>
        /// <exception cref="System.ArgumentNullException">Property name is null.</exception>
        /// <exception cref="System.ArgumentException">Property with specified name is not existed.</exception>
        /// <exception cref="System.NotSupportedException">Property cannot be written.</exception>
        T this[string propertyName] { get; set; }
    }
}
