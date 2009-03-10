using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;

namespace System.Runtime.Concepts.Constraints
{
    /// <summary>
    /// Represents constraint, which describes the concept event.
    /// This class cannot be inherited.
    /// </summary>
    sealed class EventConstraint : MemberConstraint<EventInfo>
    {
        private const BindingFlags EventFlags = BindingFlags.Instance |
            BindingFlags.Public |
            BindingFlags.NonPublic;

        /// <summary>
        /// Create constraint for the specified event.
        /// </summary>
        /// <param name="@event">Target event metadata. Cannot be null.</param>
        public EventConstraint(EventInfo @event)
            : base(@event)
        {
        }

        /// <summary>
        /// Create constraint for the event with the specified name.
        /// </summary>
        /// <param name="value">Instance of the object, which contains the event.</param>
        /// <param name="eventName">Name of the event.</param>
        public EventConstraint(object value, string eventName)
            : this(value.GetType().GetEvent(eventName, EventFlags))
        {
        }
    }
}
