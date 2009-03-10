using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents an object, which provides pattern-matching
    /// functionality
    /// </summary>
    /// <typeparam name="T">Type of the sequence</typeparam>
    public interface IPattern<TItem, TResult>
    {
        /// <summary>
        /// Provides pattern-matching of the sequence, which 
        /// describes by the navigation context
        /// </summary>
        /// <param name="context">Sequence navigation context</param>
        /// <returns>Result of the pattern matching. If value is null
        /// then input sequence doesn't match to the current pattern</returns>
        TResult Match(INavigationContext<TItem> context);

        /// <summary>
        /// Occurs when the input token stream is matched to the current
        /// pattern
        /// </summary>
        event EventHandler<MatchedEventArgs<TResult>> Matched;
    }
}
