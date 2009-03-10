using System.Collections.Generic;
using System.Collections.Generic.Extensions;
using System.Extensions;
using System.Linq;
using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a set of strings
    /// </summary>
    [Serializable]
    public sealed class StringSet : Serializable, 
        IPattern<string, IEnumerable<string>>,
        IPattern<char, IEnumerable<char>>
    {
        private readonly IEnumerable<string> m_strings;
        private EventHandler<MatchedEventArgs<IEnumerable<char>>> m_chrMatched;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private StringSet()
        {
        }

        public StringSet(params string[] strings)
            : this(true, strings)
        {
            
        }

        public StringSet(bool caseSensitive, params string[] strings)
            : this(caseSensitive, strings as IEnumerable<string>)
        {
        }

        private StringSet(bool caseSensitive, IEnumerable<string> strings)
        {
            ExceptionManager.CheckOnNull(strings, "strings");
            m_strings = strings;
            CaseSensitive = caseSensitive;
        }

        /// <summary>
        /// Get value indicating that the current lexeme is case sensitive
        /// </summary>
        public bool CaseSensitive
        {
            get;
            private set;
        }

        public string[] Strings
        {
            get { return m_strings.ToArray(); }
        }

        public static implicit operator StringSet(string[] strings)
        {
            return new StringSet(strings);
        }

        public string Match(INavigationContext<char> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            var initialPos = context.CurrentPosition;
            var result = default(string);
            foreach (var str in m_strings)
            {
                var lexeme = new Lexeme(str, CaseSensitive);
                result = lexeme.Match(context);
                if (result.IsNotNull()) break;
            }
            if (result.IsNull()) context.CurrentPosition = initialPos;
            else OnMatched(result);
            return result;
        }

        public string Match(INavigationContext<string> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            if (context.CurrentPosition.IsNull()) return null;
            var str = context.GetValueAtMarker(context.CurrentPosition);
            var newContext = new SequenceNavigator<char>(str);
            var result = Match(newContext);
            if (result.IsNotNull()) context.MoveNext();
            return result;
        }

        #region IPattern<string, IEnumerable<string>> Members

        IEnumerable<string> IPattern<string, IEnumerable<string>>.Match(INavigationContext<string> context)
        {
            return new[] { Match(context) };
        }

        public event EventHandler<MatchedEventArgs<IEnumerable<string>>> Matched;

        #endregion

        #region IPattern<char, IEnumerable<char>> Members

        IEnumerable<char> IPattern<char, IEnumerable<char>>.Match(INavigationContext<char> context)
        {
            return Match(context);
        }

        event EventHandler<MatchedEventArgs<IEnumerable<char>>> IPattern<char, IEnumerable<char>>.Matched
        {
            add { m_chrMatched += value; }
            remove { m_chrMatched -= value; }
        }

        #endregion

        #region Operators

        public static StringSet operator |(StringSet set1, StringSet set2)
        {
            ExceptionManager.CheckOnNull(set1, "set1");
            ExceptionManager.CheckOnNull(set2, "set2");
            return new StringSet(set1.CaseSensitive, set1.Strings.Concat(set2.Strings));
        }

        public static StringSet operator |(StringSet set, Lexeme lexeme)
        {
            ExceptionManager.CheckOnNull(set, "set");
            ExceptionManager.CheckOnNull(lexeme, "lexeme");
            return new StringSet(lexeme.CaseSensitive, set.Strings.AddToEnd(lexeme.ToString()));
        }

        public static Selector<string> operator |(StringSet set, IPattern<string, IEnumerable<string>> pattern)
        {
            ExceptionManager.CheckOnNull(set, "set");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<string>)
            {
                var selector = pattern.UnsafeCast<Selector<string>>();
                return selector | set;
            }
            return new Selector<string>(set, pattern);
        }

        public static Pattern<char> operator &(StringSet set, IPattern<char, IEnumerable<char>> pattern)
        {
            ExceptionManager.CheckOnNull(set, "set");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<char>)
            {
                var sequential = pattern.UnsafeCast<Pattern<char>>();
                return sequential & set;
            }
            return new Pattern<char>(set, pattern);
        }

        public static Exclude<char> operator !(StringSet set)
        {
            ExceptionManager.CheckOnNull(set, "set");
            return new Exclude<char>(set);
        }

        public static Pattern<string> operator *(StringSet token, int quantity)
        {
            ExceptionManager.CheckOnNull(token, "token");
            if (quantity < 1)
                ExceptionManager.Throw<ArgumentOutOfRangeException>("quantity");
            var iterations = new List<IPattern<string, IEnumerable<string>>>();
            for (var i = 0; i < quantity; i++)
                iterations.Add(token);
            return new Pattern<string>(iterations.ToArray());
        }

        #endregion

        private IEnumerable<string> OnMatched(string value)
        {
            var result = new[] { value };
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<string>>(result));
            if (m_chrMatched.IsNotNull())
                m_chrMatched(this, new MatchedEventArgs<IEnumerable<char>>(value));
            return result;
        }
    }
}
