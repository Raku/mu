using System.Collections.Generic;
using System.Extensions;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents X? expression
    /// </summary>
    /// <typeparam name="T">Type of the inner rule</typeparam>
    [Serializable]
    public sealed class ZeroOrOne<T> : Quantifier<T>
    {
        private readonly IPattern<T, IEnumerable<T>> m_pattern;

        public ZeroOrOne(IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            m_pattern = pattern;
        }

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private ZeroOrOne()
        {
        }

        /// <summary>
        /// Represents a pattern on which quantifier performed
        /// </summary>
        public override IPattern<T, IEnumerable<T>> Pattern
        {
            get { return m_pattern; }
        }

        public override IEnumerable<T> Match(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            //If navigator is empty then return empty token
            if (context.CurrentPosition.IsNull())
                return new T[] { Filler };
            var result = Pattern.Match(context);
            return OnMatched(result);
        }

        /// <summary>
        /// Occurs when the input token is matched to the current quantifier
        /// </summary>
        public override event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        private IEnumerable<T> OnMatched(IEnumerable<T> value)
        {
            var result = value.IsNull() ? Filler.IsNull() ? null : new T[] { Filler } : value;
            if (Matched.IsNotNull() && result.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
            return result;
        }

        #region Operators

        public static Selector<T> operator |(ZeroOrOne<T> token, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(token, "token");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<T>)
            {
                var selector = pattern.UnsafeCast<Selector<T>>();
                return selector | token;
            }
            return new Selector<T>(token, pattern);
        }

        public static Pattern<T> operator &(ZeroOrOne<T> token, IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(token, "token");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<T>)
            {
                var sequential = pattern.UnsafeCast<Pattern<T>>();
                return sequential & token;
            }
            return new Pattern<T>(token, pattern);
        }

        public static ZeroOrOne<T> operator !(ZeroOrOne<T> token)
        {
            //!(a?) = (!a)?
            ExceptionManager.CheckOnNull(token, "token");
            return new ZeroOrOne<T>(new Exclude<T>(token.Pattern));
        }

        public static Pattern<T> operator *(ZeroOrOne<T> token, int quantity)
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
