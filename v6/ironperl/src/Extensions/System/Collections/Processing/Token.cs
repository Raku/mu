using System.Collections.Generic;
using System.Extensions;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a simple token matching
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public sealed class Token<T> : Serializable, IPattern<T, IEnumerable<T>>
    {
        private readonly T m_value;
        private IEqualityComparer<T> m_comparer;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private Token()
        {
        }

        /// <summary>
        /// Create a token with single value
        /// </summary>
        /// <param name="value"></param>
        public Token(T value)
            : this(value, null)
        {
        }

        /// <summary>
        /// Create a token with single value
        /// </summary>
        /// <param name="value"></param>
        /// <param name="comparer"></param>
        public Token(T value, IEqualityComparer<T> comparer)
        {
            if (comparer.IsNull()) comparer = new DefaultComparer<T>();
            m_value = value;
            m_comparer = comparer;
        }

        public IEnumerable<T> Match(INavigationContext<T> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            //If navigator is empty then pattern is not applicable
            if (context.CurrentPosition.IsNull()) return null;
            var extrnValue = context.GetValueAtMarker(context.CurrentPosition);
            if (CompareValues(m_value, extrnValue))
            {
                context.MoveNext();
                return OnMatched(m_value);
            }
            return null;
        }

        /// <summary>
        /// Occurs when the input token stream is resolved as a valid token
        /// </summary>
        public event EventHandler<MatchedEventArgs<IEnumerable<T>>> Matched;

        private IEnumerable<T> OnMatched(T value)
        {
            var result = new[] { value };
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<T>>(result));
            return result;
        }

        private bool CompareValues(T value1, T value2)
        {
            return m_comparer.Equals(value1, value2);
        }

        /// <summary>
        /// Get token value
        /// </summary>
        public T Value
        {
            get { return m_value; }
        }

        #region Operators

        public static implicit operator Token<T>(T value)
        {
            return new Token<T>(value);
        }

        public static Selector<T> operator |(Token<T> token, IPattern<T, IEnumerable<T>> pattern)
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

        public static Pattern<T> operator &(Token<T> token, IPattern<T, IEnumerable<T>> pattern)
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

        public static Exclude<T> operator !(Token<T> token)
        {
            ExceptionManager.CheckOnNull(token, "token");
            return new Exclude<T>(token);
        }

        public static Pattern<T> operator *(Token<T> token, int quantity)
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
