using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Collections.Processing
{
    using ExceptionManager = global::System.Extensions.ExceptionManager;

    /// <summary>
    /// Represents extension methods for System.Collections.Processing.IPattern interface
    /// </summary>
    [TypeExtender(typeof(IPattern<,>))]
	public static class IPatternExtensions
	{
        /// <summary>
        /// Create regular expression like A?
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="pattern"></param>
        /// <returns></returns>
        public static Quantifier<T> ZeroOrOne<T>(this IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            return new ZeroOrOne<T>(pattern);
        }

        /// <summary>
        /// Create Kleene closure for specified pattern
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="pattern"></param>
        /// <returns></returns>
        public static Quantifier<T> ZeroOrMore<T>(this IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            return new KleeneClosure<T>(pattern);
        }

        /// <summary>
        /// Create positive closure
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="pattern"></param>
        /// <returns></returns>
        public static Quantifier<T> OneOrMore<T>(this IPattern<T, IEnumerable<T>> pattern)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            return new PositiveClosure<T>(pattern);
        }

        /// <summary>
        /// Provides pattern-mathcing of the strongly-typed sequence
        /// </summary>
        /// <typeparam name="T">Type of the sequence</typeparam>
        /// <param name="sequence">Sequence to be matched</param>
        /// <returns></returns>
        public static IEnumerable<T> Match<T>(this IPattern<T, IEnumerable<T>> pattern, IEnumerable<T> sequence)
        {
            ExceptionManager.CheckOnNull(pattern, "pattern");
            ExceptionManager.CheckOnNull(sequence, "sequence");
            return pattern.Match(new SequenceNavigator<T>(sequence));
        }
	}
}
