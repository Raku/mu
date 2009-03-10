using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.Serialization;
using System.Extensions;

namespace System.Collections.Processing
{
    using ExceptionManager = global::System.Extensions.ExceptionManager;

    /// <summary>
    /// Represents a Kleene closure
    /// </summary>
    [Serializable]
    public sealed class KleeneClosure<T> : Quantifier<T>
    {
        private readonly IPattern<T, IEnumerable<T>> m_pattern;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private KleeneClosure()
        {
        }

        /// <summary>
        /// Create a Kleene closure for specified pattern
        /// </summary>
        /// <param name="pattern"></param>
        public KleeneClosure(IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            m_pattern = pattern;
        }

        public override IEnumerable<T> Match(INavigationContext<T> context)
        {
            var result = InternalMatch(context);
            if (result.IsNotNull()) OnMatched(result);
            return result;
        }

        /// <summary>
        /// Occurs when the input token stream is validated by the current closure
        /// </summary>
        public override event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        private IEnumerable<T> InternalMatch(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            var initialPos = context.CurrentPosition;
            var hasResult = false;
            while (true)
            {
                var previous = context.CurrentPosition;
                var match = Pattern.Match(context);
                if (hasResult = match.IsNotNull())
                    foreach (var item in match) yield return item;
                else break;
                if (previous.Equals(context.CurrentPosition)) break;
            }
            if (hasResult.IsFalse())
            {
                context.CurrentPosition = initialPos;
                if (Filler.IsNotNull()) yield return Filler;
            }
        }

        private void OnMatched(IEnumerable<T> result)
        {
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
        }

        /// <summary>
        /// Get pattern, which is used for Kleene closure
        /// </summary>
        public override IPattern<T, IEnumerable<T>> Pattern
        {
            get { return m_pattern; }
        }

        #region Operators

        public static Selector<T> operator |(KleeneClosure<T> kleene, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(kleene, "kleene");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<T>)
            {
                var selector = pattern.UnsafeCast<Selector<T>>();
                return selector | kleene;
            }
            return new Selector<T>(kleene, pattern);
        }

        public static Pattern<T> operator &(KleeneClosure<T> kleene, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(kleene, "kleene");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<T>)
            {
                var sequential = pattern.UnsafeCast<Pattern<T>>();
                return sequential & kleene;
            }
            return new Pattern<T>(kleene, pattern);
        }

        public static KleeneClosure<T> operator !(KleeneClosure<T> kleene)
        {
            //!(a*) = (!a)*
            ExceptionManager.CheckOnNull(kleene, "kleene");
            return new KleeneClosure<T>(new Exclude<T>(kleene.Pattern));
        }

        public static Pattern<T> operator *(KleeneClosure<T> token, int quantity)
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
