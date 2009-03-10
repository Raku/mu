using System.Collections.Generic;
using System.Collections.Generic.Extensions;
using System.Extensions;
using System.Linq;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a BNF expression A | B | C |...| Z
    /// </summary>
    public sealed class Selector<T>: Serializable, IPattern<T, IEnumerable<T>>
    {
        private readonly IEnumerable<IPattern<T, IEnumerable<T>>> m_values;
        private IEqualityComparer<T> m_comparer;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private Selector()
        {
        }

        /// <summary>
        /// Create a selection choice for specified lexemes
        /// </summary>
        /// <param name="values"></param>
        public Selector(params IPattern<T, IEnumerable<T>>[] values)
            :this(null, values)
        {
        }

        private Selector(IEqualityComparer<T> comparer, IEnumerable<IPattern<T, IEnumerable<T>>> values)
        {
            ExceptionManager.CheckOnNull(values, "values");
            if (comparer.IsNull()) comparer = new DefaultComparer<T>();
            m_values = values;
            m_comparer = comparer;
        }

        /// <summary>
        /// Create a selection choice for specified lexemes
        /// </summary>
        /// <param name="values"></param>
        public Selector(IEqualityComparer<T> comparer, params IPattern<T, IEnumerable<T>>[] values)
            : this(comparer, values as IEnumerable<IPattern<T, IEnumerable<T>>>)
        {
            
        }

        public IEnumerable<T> Match(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            //Save initial position of the navigator
            var initialPos = context.CurrentPosition;
            var result = default(IEnumerable<T>);
            //Iterate through all patterns
            foreach (var pattern in m_values)
            {
                result = pattern.Match(context);
                if (result.IsNotNull()) break;
            }
            if (result.IsNull()) context.CurrentPosition = initialPos;
            else OnMatched(result);
            return result;
        }

        /// <summary>
        /// Occurs when the input token stream is matched to the current pattern
        /// </summary>
        public event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        private void OnMatched(IEnumerable<T> result)
        {
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
        }

        /// <summary>
        /// Get inner patterns
        /// </summary>
        public IEnumerable<IPattern<T, IEnumerable<T>>> Patterns
        {
            get { return m_values; }
        }

        public static implicit operator Selector<T>(IPattern<T, IEnumerable<T>>[] values)
        {
            return new Selector<T>(values);
        }

        #region Operators

        /// <summary>
        /// Add specified pattern to the current selector
        /// </summary>
        /// <param name="choice">Target selector</param>
        /// <param name="pattern">Pattern to be added</param>
        /// <returns>Modified selector</returns>
        public static Pattern<T> operator &(Selector<T> choice, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(choice, "choice");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<T>)
            {
                var sequential = pattern.UnsafeCast<Pattern<T>>();
                return sequential & choice;
            }
            return new Pattern<T>(choice, pattern);
        }

        public static Selector<T> operator |(Selector<T> choice, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(choice, "choice");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<T>)
            {
                var choice2 = pattern.UnsafeCast<Selector<T>>();
                return new Selector<T>(choice.m_comparer, choice.Patterns.Concat(choice2.Patterns));
            }
            return new Selector<T>(choice.m_comparer, choice.Patterns.AddToEnd(pattern));
        }

        public static Exclude<T> operator !(Selector<T> selection)
        {
            ExceptionManager.CheckOnNull(selection, "selection");
            return new Exclude<T>(selection);
        }

        public static Pattern<T> operator *(Selector<T> token, int quantity)
        {
            ExceptionManager.CheckOnNull(token, "token");
            if (quantity < 1)
                ExceptionManager.Throw<ArgumentOutOfRangeException>("quantity");
            var iterations = new List<IPattern<T, IEnumerable<T>>>();
            for (var i = 0; i < quantity; i++)
                iterations.Add(token);
            return new Pattern<T>(iterations.ToArray());
        }

        #endregion
    }
}
