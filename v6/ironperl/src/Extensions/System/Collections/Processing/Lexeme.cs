using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Extensions;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a string terminal symbol
    /// </summary>
    /// <remarks>About terminals and non-terminals, 
    /// see Red Dragon Book of J. Ulman, A. Aho, R. Seti</remarks>
    [Serializable]
    [DebuggerDisplay("Lexeme = {m_value}")]
    public class Lexeme: Serializable, 
        IPattern<char, IEnumerable<char>>,
        IPattern<string, IEnumerable<string>>
    {
        private readonly string m_value;
        private EventHandler<MatchedEventArgs<IEnumerable<char>>> m_strMatched;

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private Lexeme()
        {
        }

        /// <summary>
        /// Create string terminal with specified value
        /// </summary>
        /// <param name="value">String literal</param>
        /// <param name="caseSensitive">Indicates that the current lexeme
        /// is case sensitive</param>
        public Lexeme(string value, bool caseSensitive)
        {
            CaseSensitive = caseSensitive;
            m_value = value;
        }

        /// <summary>
        /// Create string terminal from singel character
        /// </summary>
        /// <param name="value">Character literal</param>
        /// <param name="caseSensitive">Indicates that the current lexeme
        /// is case sensitive</param>
        public Lexeme(char value, bool caseSensitive)
            : this(value.ToString(), caseSensitive)
        {
        }

        /// <summary>
        /// Create string terminal with specified value
        /// </summary>
        /// <param name="value">String literal</param>
        public Lexeme(string value)
            : this(value, false)
        {
        }

        /// <summary>
        /// Create string terminal from singel character
        /// </summary>
        /// <param name="value">Character literal</param>
        public Lexeme(char value)
            : this(value.ToString())
        {
        }

        private bool CompareChars(char ch1, char ch2)
        {
            if (CaseSensitive.IsFalse())
            {
                ch1 = char.ToLower(ch1);
                ch2 = char.ToLower(ch2);
            }
            return ch1 == ch2;
        }

        /// <summary>
        /// Returns a string literal, which identifies the current lexeme
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return m_value;
        }

        /// <summary>
        /// Get value indicating that the current lexeme is case sensitive
        /// </summary>
        public bool CaseSensitive
        {
            get;
            private set;
        }

        public static implicit operator Lexeme(char character)
        {
            return new Lexeme(character);
        }

        public static implicit operator Lexeme(string literal)
        {
            return new Lexeme(literal);
        }

        public string Match(INavigationContext<char> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            object currentPos, initalPos = currentPos = context.CurrentPosition;
            var result = new StringBuilder();
            foreach (var character in m_value)
            {
                //If navigator is break then exit from method
                if (currentPos.IsNull())
                {
                    context.CurrentPosition = initalPos;
                    return null;
                }
                //Get character from navigator
                var character2 = context.GetValueAtMarker(context.CurrentPosition);
                //If characters are not equal then exit from method
                if (CompareChars(character, character2).IsFalse())
                {
                    context.CurrentPosition = initalPos;
                    return null;
                }
                result.Append(character2);
                //Go to next character in navigator
                currentPos = context.MoveNext();
            }
            return result.ToString();
        }

        public string Match(INavigationContext<string> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            if (context.CurrentPosition.IsNull()) return null;
            var str = context.GetValueAtMarker(context.CurrentPosition);
            var newContext = new SequenceNavigator<char>(str);
            var result = Match(newContext);
            if (result.IsNotNull())
            {
                OnMatched(result);
                context.MoveNext();
            }
            return result;
        }

        #region IPattern<char, IEnumerable<char>> Members

        IEnumerable<char> IPattern<char, IEnumerable<char>>.Match(INavigationContext<char> context)
        {
            return Match(context);
        }

        event EventHandler<MatchedEventArgs<IEnumerable<char>>> IPattern<char, IEnumerable<char>>.Matched
        {
            add { m_strMatched += value; }
            remove { m_strMatched -= value; }
        }

        #endregion

        #region IPattern<string, IEnumerable<string>> Members

        IEnumerable<string> IPattern<string, IEnumerable<string>>.Match(INavigationContext<string> context)
        {
            return new[] { Match(context) };
        }
        

        /// <summary>
        /// Occurs when the input character stream is resolved as a valid string
        /// </summary>
        public event EventHandler<MatchedEventArgs<IEnumerable<string>>> Matched;

        #endregion

        private void OnMatched(string result)
        {
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<IEnumerable<string>>(new[] { result }));
            if (m_strMatched.IsNotNull())
                m_strMatched(this, new MatchedEventArgs<IEnumerable<char>>(result));
        }

        #region Operators

        public static StringSet operator |(Lexeme lexeme1, Lexeme lexeme2)
        {
            ExceptionManager.CheckOnNull(lexeme1, "lexeme1");
            ExceptionManager.CheckOnNull(lexeme2, "lexeme2");
            return new StringSet(lexeme1.CaseSensitive, lexeme1.ToString(), lexeme2.ToString());
        }

        public static Selector<char> operator |(Lexeme token, IPattern<char, IEnumerable<char>> pattern)
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

        public static Pattern<char> operator &(Lexeme token, IPattern<char, IEnumerable<char>> pattern)
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

        public static Pattern<string> operator +(Lexeme token, IPattern<string, IEnumerable<string>> pattern)
        {
            ExceptionManager.CheckOnNull(token, "token");
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Pattern<string>)
            {
                var sequential = pattern.UnsafeCast<Pattern<string>>();
                return sequential & token;
            }
            return new Pattern<string>(token, pattern);
        }

        public static Exclude<char> operator !(Lexeme token)
        {
            ExceptionManager.CheckOnNull(token, "token");
            return new Exclude<char>(token);
        }

        public static Exclude<string> operator ~(Lexeme token)
        {
            ExceptionManager.CheckOnNull(token, "token");
            return new Exclude<string>(token);
        }

        public static Pattern<char> operator *(Lexeme lexeme, int quantity)
        {
            ExceptionManager.CheckOnNull(lexeme, "lexeme");
            if (quantity < 1)
                ExceptionManager.Throw<ArgumentOutOfRangeException>("quantity");
            var iterations = new List<IPattern<char, IEnumerable<char>>>();
            for (var i = 0; i < quantity; i++)
                iterations.Add(lexeme);
            return new Pattern<char>(iterations.ToArray());
        }

        #endregion

        public Selector<string> Or(IPattern<string, IEnumerable<string>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            if (pattern is Selector<string>)
            {
                var selector = pattern.UnsafeCast<Selector<string>>();
                return selector | (this as IPattern<string, IEnumerable<string>>);
            }
            return new Selector<string>(this as IPattern<string, IEnumerable<string>>, pattern);
        }
    }
}
