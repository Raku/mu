using System.Collections.Generic;
using System.Collections.Generic.Extensions;
using System.Extensions;
using System.Linq;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Provides an ultimate class for building patterns
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public sealed class Pattern<T> : Serializable, IPattern<T, IEnumerable<T>>
    {
        private readonly IEnumerable<IPattern<T, IEnumerable<T>>> m_objects;

        sealed class MatchResult
        {
            private bool m_hasResult;

            public bool HasResult
            {
                get { return m_hasResult; }
                set { m_hasResult = value; }
            }
        }

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private Pattern()
        {
        }

        /// <summary>
        /// Create sequential pattern. Instance of the input value must be convertible
        /// to T or IPattern types
        /// </summary>
        /// <param name="args">Arguments to be matched</param>
        public Pattern(params IPattern<T, IEnumerable<T>>[] args)
            : this(args as IEnumerable<IPattern<T, IEnumerable<T>>>)
        {
            
        }

        private Pattern(IEnumerable<IPattern<T, IEnumerable<T>>> args)
        {
            ExceptionManager.CheckOnNull(args, "args");
            m_objects = args;
            LazyMatch = false;
        }

        /// <summary>
        /// Get or set lazy match option
        /// </summary>
        public bool LazyMatch
        {
            get;
            set;
        }

        /// <summary>
        /// Provides pattern-matching of the sequence, which 
        /// describes by the navigation context
        /// </summary>
        /// <param name="context">Sequence navigation context</param>
        /// <returns>Resultof the pattern matching. If value is null
        /// then input sequence doesn't match to the current pattern</returns>
        public IEnumerable<T> Match(INavigationContext<T> context)
        {
            var result = InternalMatch(context);
            if (result.IsNotNull()) OnMatched(result);
            return result;
        }

        private IEnumerable<T> InternalMatch(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            var initialPos = context.CurrentPosition;
            foreach (var pattern in m_objects)
            {
                var match = pattern.Match(context);
                //if one of the specified patterns is not matched to input data
                //then break matching and restore navigator position
                if (match.IsNull())
                {
                    context.CurrentPosition = initialPos;
                    yield break;
                }
                foreach (var item in match)
                    yield return item;
            }
        }

        /// <summary>
        /// Occures then input token stream is matched to the current pattern
        /// </summary>
        public event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        public IEnumerable<IPattern<T, IEnumerable<T>>> Patterns
        {
            get { return m_objects; }
        }

        private void OnMatched(IEnumerable<T> result)
        {
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
        }

        #region Operators

        /// <summary>
        /// Add specified pattern to the current sequential pattern
        /// </summary>
        /// <param name="choice">Target selector</param>
        /// <param name="pattern">Pattern to be added</param>
        /// <returns>Modified selector</returns>
        public static Pattern<T> operator &(Pattern<T> pattern1, IPattern<T, IEnumerable<T>> pattern2)
        {
            ExceptionManager.CheckOnNull(pattern1, "pattern1");
            ExceptionManager.CheckOnNull(pattern2, "pattern2");
            if (pattern2 is Pattern<T>)
            {
                var sequential = pattern2.UnsafeCast<Pattern<T>>();
                return new Pattern<T>(pattern1.Patterns.Concat(sequential.Patterns));
            }
            return new Pattern<T>(pattern1.Patterns.AddToEnd(pattern2));
        }

        public static Selector<T> operator |(Pattern<T> sequential, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(sequential, "sequential");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<T>)
            {
                var selector = pattern.UnsafeCast<Selector<T>>();
                return selector | sequential;
            }
            return new Selector<T>(sequential, pattern);
        }

        public static Exclude<T> operator !(Pattern<T> sequential)
        {
            ExceptionManager.CheckOnNull(sequential, "sequential");
            return new Exclude<T>(sequential);
        }

        public static Pattern<T> operator *(Pattern<T> pattern, int quantity)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (quantity < 1)
                ExceptionManager.Throw<ArgumentOutOfRangeException>("quantity");
            var iterations = new List<IPattern<T, IEnumerable<T>>>();
            for (var i = 0; i < quantity; i++)
                iterations.Add(pattern);
            return new Pattern<T>(iterations.ToArray());
        }

        #endregion
    }
}
