using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.Serialization;

//Aliases
using BindingFlags = System.Reflection.BindingFlags;

namespace System.Runtime.Serialization
{
    /// <summary>
    /// Represents an ultimate class for any object, which wants to use
    /// automatic serialization of all fields (include read-only fields)
    /// </summary>
    [Serializable]
    public abstract class Serializable
    {
        /// <summary>
        /// Create instance of the serializable objects
        /// </summary>
        protected Serializable()
        {
        }        

        /// <summary>
        /// Method is called during deserialization of an object.
        /// </summary>
        /// <param name="context"></param>
        protected virtual void OnBeforeDeserialization(StreamingContext context)
        {

        }

        [OnDeserializing]
        private void BeforeDeserialization(StreamingContext context)
        {
            OnBeforeDeserialization(context);
        }

        /// <summary>
        /// Method is called immediately after deserialization of the object.
        /// </summary>
        /// <param name="context"></param>
        protected virtual void OnAfterDeserialization(StreamingContext context)
        {

        }

        [OnDeserialized]
        private void AfterDeserialization(StreamingContext context)
        {
            OnAfterDeserialization(context);
        }
    }
}
