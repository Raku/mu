using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a basic class for all quantifiers. Quantifier
    /// doesn't operate with the data, it operates with specified pattern
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public abstract class Quantifier<T> : Serializable, IPattern<T, IEnumerable<T>>
    {
        protected Quantifier()
        {
            Filler = default(T);
        }

        /// <summary>
        /// Represents a filler, which is used when quantifier has no matching
        /// </summary>
        protected T Filler
        {
            get;
            set;
        }

        /// <summary>
        /// Represents a pattern on which quantifier performed
        /// </summary>
        public abstract IPattern<T, IEnumerable<T>> Pattern
        {
            get;
        }

        #region IPattern<T, IEnumerable<T>> Members

        public abstract IEnumerable<T> Match(INavigationContext<T> context);

        public abstract event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        #endregion
    }
}
