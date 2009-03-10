using System.Collections.Generic;
using System.Extensions;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a letter
    /// </summary>
    [Serializable]
    public sealed class Letter : Serializable, IPattern<char, IEnumerable<char>>
    {
        /// <summary>
        /// Create pattern for digit
        /// </summary>
        public Letter()
        {
        }

        private IEnumerable<char> OnMatched(char value)
        {
            var result = new[] { value };
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<char>>(result));
            return result;
        }

        #region IPattern<char> Members

        public IEnumerable<char> Match(INavigationContext<char> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            if (context.CurrentPosition.IsNull()) return null;
            var character = context.GetValueAtMarker(context.CurrentPosition);
            if (char.IsLetter(character))
            {
                context.MoveNext();
                return OnMatched(character);
            }
            return null;
        }

        /// <summary>
        /// Occurs when the input character stream is resolved as Unicode letter
        /// </summary>
        public event EventHandler<MatchedEventArgs<IEnumerable<char>>> Matched;

        #endregion

        #region Operators

        public static Selector<char> operator |(Letter token, IPattern<char, IEnumerable<char>> pattern)
        {
            ExceptionManager.CheckOnNull(token, "token");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<char>)
            {
                var selector = pattern.UnsafeCast<Selector<char>>();
                return selector | token;
            }
            return new Selector<char>(token, pattern);
        }

        public static Pattern<char> operator &(Letter token, IPattern<char, IEnumerable<char>> pattern)
        {
            ExceptionManager.CheckOnNull(token, "token");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<char>)
            {
                var sequential = pattern.UnsafeCast<Pattern<char>>();
                return sequential & token;
            }
            return new Pattern<char>(token, pattern);
        }

        public static Exclude<char> operator !(Letter token)
        {
            ExceptionManager.CheckOnNull(token, "token");
            return new Exclude<char>(token);
        }

        public static Pattern<char> operator *(Letter token, int quantity)
        {
            ExceptionManager.CheckOnNull(token, "token");
            if (quantity < 1)
                ExceptionManager.Throw<ArgumentOutOfRangeException>("quantity");
            var iterations = new List<IPattern<char, IEnumerable<char>>>();
            for (var i = 0; i < quantity; i++)
                iterations.Add(token);
            return new Pattern<char>(iterations.ToArray());
        }

        #endregion
    }
}
