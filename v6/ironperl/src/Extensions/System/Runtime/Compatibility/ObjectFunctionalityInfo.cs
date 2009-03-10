using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;
using System.ComponentModel;

namespace System.Runtime.Compatibility
{
    /// <summary>
    /// Represents an ultimate class for building of object functionality 
    /// description.
    /// </summary>
    public abstract class ObjectFunctionalityInfo: IObjectFunctionality
    {
        private PropertyChangedEventHandler m_propertyChangedEvent;
        private PropertyChangingEventHandler m_propertyChangingEvent;

        /// <summary>
        /// Initialize instance of the System.Runtime.Compatibility.ObjectFunctionalityInfo
        /// class.
        /// </summary>
        protected ObjectFunctionalityInfo()
        {

        }

        #region IObjectWithProperties Members

        string[] IObjectWithProperties<object>.Properties
        {
            get { return Functions; }
        }

        /// <summary>
        /// Get functionality description by it name.
        /// </summary>
        /// <param name="functionName">Name of the object function.</param>
        /// <returns>Description of the functionality.</returns>
        /// <exception cref="System.ArgumentNullException">Function name is null.</exception>
        /// <exception cref="System.ArgumentException">Property with specified name is not exsited.</exception>
        /// <exception cref="System.NotSupportedException">Set accessor is not supported.</exception>
        /// <remarks>Set accessor for the current property is not supported.</remarks>
        public object this[string functionName]
        {
            get
            {
                if (functionName.IsNull())
                    throw new ArgumentNullException("functionName");
                var property = GetType().GetProperty(functionName);
                if (property.IsNull())
                    throw new ArgumentException(
                        String.Format(Properties.Resources.String_PropertyIsNotExisted, functionName),
                        "functionName");
                return property.GetValue(this, null);
            }
        }

        #endregion

        /// <summary>
        /// Get name of the all functions, which is supported by the object.
        /// </summary>
        /// <remarks>This property returns an array of the all public properties
        /// inside of the current instance.</remarks>
        public string[] Functions
        {
            get
            {
                var currentType = typeof(ObjectFunctionalityInfo);
                var queryResult = from property in GetType().GetProperties()
                                  where !property.DeclaringType.Equals(currentType)
                                  select property.Name;
                return queryResult.ToArray();
            }
        }

        #region IObjectWithProperties Members

        object IObjectWithProperties<object>.this[string propertyName]
        {
            get
            {
                return this[propertyName];
            }
            [NotSupported]
            set
            {
                throw new NotSupportedException();
            }
        }

        #endregion

        /// <summary>
        /// Fires the PropertyChanged event.
        /// </summary>
        /// <param name="propertyName">Name of the property, which value is changed.</param>
        protected void OnPropertyChanged(string propertyName)
        {
            if (m_propertyChangedEvent.IsNotNull())
                m_propertyChangedEvent(this,
                    new PropertyChangedEventArgs(propertyName));
        }

        protected void OnPropertyChanging(string propertyName)
        {
            if (m_propertyChangingEvent.IsNotNull())
                m_propertyChangingEvent(this,
                    new PropertyChangingEventArgs(propertyName));
        }

        #region INotifyPropertyChanged Members

        /// <summary>
        /// Occurs when a property value changes.
        /// </summary>
        event PropertyChangedEventHandler INotifyPropertyChanged.PropertyChanged
        {
            add { m_propertyChangedEvent += value; }
            remove { m_propertyChangedEvent -= value; }
        }

        #endregion

        #region INotifyPropertyChanging Members

        /// <summary>
        /// Occurs when a property value is changing.
        /// </summary>
        event PropertyChangingEventHandler INotifyPropertyChanging.PropertyChanging
        {
            add { m_propertyChangingEvent += value; }
            remove { m_propertyChangingEvent -= value; }
        }

        #endregion
    }
}
