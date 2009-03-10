using System.Extensions;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a custom pattern
    /// </summary>
    /// <typeparam name="TItem"></typeparam>
    /// <typeparam name="TResult"></typeparam>
    [Serializable]
    public class Rule<TItem, TResult> : Serializable, IPattern<TItem, TResult>
    {
        private readonly Converter<TItem, TResult> m_action;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        protected Rule()
        {
        }

        /// <summary>
        /// Create a custom parser from specified lambda or delegate
        /// </summary>
        /// <param name="action"></param>
        public Rule(Converter<TItem, TResult> action)
        {
            ExceptionManager.CheckOnNull(action, "action");
            m_action = action;
        }

        #region IPattern<TItem,TResult> Members

        public TResult Match(INavigationContext<TItem> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            if (context.CurrentPosition.IsNull()) return default(TResult);
            var obj = context.GetValueAtMarker(context.CurrentPosition);
            var result = Action(obj);
            if (result.IsNotNull()) OnMatched(result);
            return result;
        }

        public event EventHandler<MatchedEventArgs<TResult>> Matched;

        #endregion

        /// <summary>
        /// Get code that implements patternn matching logic
        /// </summary>
        public Converter<TItem, TResult> Action
        {
            get { return m_action; }
        }

        private void OnMatched(TResult item)
        {
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<TResult>(item));
        }
    }
}
