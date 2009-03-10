using System.Collections.Generic;
using System.Extensions;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a custom pattern
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public sealed class Parser<T>: Parser<T, T>
    {
        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private Parser()
        {
        }

        /// <summary>
        /// Create a custom pattern with specified lambda or action
        /// </summary>
        /// <param name="action"></param>
        public Parser(Converter<T, T> action)
            : base(action)
        {
        }

        #region Operators

        public static implicit operator Parser<T>(Converter<T, T> action)
        {
            return new Parser<T>(action);
        }

        public static Selector<T> operator |(Parser<T> token, IPattern<T, IEnumerable<T>> pattern)
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

        public static Pattern<T> operator &(Parser<T> token, IPattern<T, IEnumerable<T>> pattern)
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

        public static Exclude<T> operator !(Parser<T> token)
        {
            ExceptionManager.CheckOnNull(token, "token");
            return new Exclude<T>(token);
        }

        public static Pattern<T> operator *(Parser<T> token, int quantity)
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

    /// <summary>
    /// Represents a custom pattern
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public class Parser<T, G> : Rule<T, IEnumerable<G>>
    {
        /// <summary>
        /// Deserialization constructor
        /// </summary>
        protected Parser()
        {
        }

        /// <summary>
        /// Create a custom pattern with specified lambda or action
        /// </summary>
        /// <param name="action"></param>
        public Parser(Converter<T, G> action)
            : base(t => new[] { action(t) })
        {
        }

        public static implicit operator Parser<T, G>(Converter<T, G> action)
        {
            return new Parser<T, G>(action);
        }
    }
}
