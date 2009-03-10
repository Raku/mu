using System.Collections.Generic;
using System.Extensions;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a positive closure
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public sealed class PositiveClosure<T>: Quantifier<T>
    {
        private readonly IPattern<T, IEnumerable<T>> m_pattern;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private PositiveClosure()
        {
        }

        /// <summary>
        /// Create positive closure for specified pattern
        /// </summary>
        /// <param name="pattern"></param>
        public PositiveClosure(IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            m_pattern = pattern;
        }

        /// <summary>
        /// Get pattern for the current closure
        /// </summary>
        public override IPattern<T, IEnumerable<T>> Pattern
        {
            get { return m_pattern; }
        }

        public override IEnumerable<T> Match(INavigationContext<T> context)
        {
            var result = InternalMatch(context);
            if (result.IsNotNull()) OnMatched(result);
            return result;
        }

        public override event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        private IEnumerable<T> InternalMatch(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            if (context.CurrentPosition.IsNull())
            {
                if (Filler.IsNotNull()) yield return Filler;
                yield break;
            }
            var result = Pattern.Match(context);
            if (result.IsNull())
            {
                if (Filler.IsNotNull()) yield return Filler;
                yield break;
            }
            var initialPos = context.CurrentPosition;
            //Apply Kleene closure
            while (true)
            {
                var previous = context.CurrentPosition;
                var match = Pattern.Match(context);
                if (match.IsNotNull())
                    foreach (var item in match) yield return item;
                else break;
                if (previous.Equals(context.CurrentPosition)) break;
            }
        }

        private void OnMatched(IEnumerable<T> result)
        {
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
        }

        #region Operators

        public static Selector<T> operator |(PositiveClosure<T> closure, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(closure, "closure");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<T>)
            {
                var selector = pattern.UnsafeCast<Selector<T>>();
                return selector | closure;
            }
            return new Selector<T>(closure, pattern);
        }

        public static Pattern<T> operator &(PositiveClosure<T> closure, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(closure, "closure");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<T>)
            {
                var sequential = pattern.UnsafeCast<Pattern<T>>();
                return sequential & closure;
            }
            return new Pattern<T>(closure, pattern);
        }

        public static PositiveClosure<T> operator !(PositiveClosure<T> closure)
        {
            //!(a+) = !(aa*) = !a(!a)* <=> b=!a <=> !(a+) = bb* = b+ = (!a)+
            ExceptionManager.CheckOnNull(closure, "closure");
            return new PositiveClosure<T>(new Exclude<T>(closure.Pattern));
        }

        public static Pattern<T> operator *(PositiveClosure<T> closure, int quantity)
        {
            ExceptionManager.CheckOnNull(closure, "closure");
            if (quantity < 1)
                ExceptionManager.Throw<ArgumentOutOfRangeException>("quantity");
            var iterations = new List<IPattern<T, IEnumerable<T>>>();
            for (var i = 0; i < quantity; i++)
                iterations.Add(closure);
            return new Pattern<T>(iterations.ToArray());
        }

        #endregion
    }
}
