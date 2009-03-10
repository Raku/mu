using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Extensions;

namespace System.Collections.Processing
{
    using ExceptionManager = global::System.Extensions.ExceptionManager;

    /// <summary>
    /// Represents exclude pattern
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public sealed class Exclude<T>: Serializable, IPattern<T, IEnumerable<T>>
    {
        private readonly IPattern<T, IEnumerable<T>> m_pattern;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private Exclude()
        {
        }

        /// <summary>
        /// Create pattern
        /// </summary>
        /// <param name="innerPattern"></param>
        public Exclude(IPattern<T, IEnumerable<T>> innerPattern)
        {
            ExceptionManager.CheckOnNull(innerPattern, "innerPattern");
            m_pattern = innerPattern;
        }

        public IPattern<T, IEnumerable<T>> Pattern
        {
            get { return m_pattern; }
        }

        #region IPattern<T, IEnumerable<T>> Members

        public IEnumerable<T> Match(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            if (context.CurrentPosition.IsNull()) return null;
            var initialPos = context.CurrentPosition;
            var result = Pattern.Match(context);
            if (result.IsNull()) OnMatched(context.GetValueAtMarker(initialPos));
            //Rollback navigator state
            context.CurrentPosition = initialPos;
            return null;
        }

        /// <summary>
        /// Occures when input token stream contains valid token
        /// </summary>
        public event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        #endregion

        #region Operators

        public static Selector<T> operator |(Exclude<T> token, IPattern<T, IEnumerable<T>> pattern)
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

        public static Pattern<T> operator &(Exclude<T> token, IPattern<T, IEnumerable<T>> pattern)
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

        public static IPattern<T, IEnumerable<T>> operator !(Exclude<T> token)
        {
            ExceptionManager.CheckOnNull(token, "token");
            return token.Pattern;
        }

        public static Pattern<T> operator *(Exclude<T> token, int quantity)
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

        private IEnumerable<T> OnMatched(T value)
        {
            var result = new[] { value };
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
            return result;
        }
    }
}
