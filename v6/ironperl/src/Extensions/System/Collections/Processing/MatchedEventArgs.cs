using System.Runtime.Serialization;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents arguments for System.Collections.Processing.IPattern.Matched event
    /// </summary>
    [Serializable]
    public sealed class MatchedEventArgs<TResult>: EventArgs
    {
        private readonly TResult m_result;

        /// <summary>
        /// Create event arguments
        /// </summary>
        /// <param name="matchResult">Match result</param>
        public MatchedEventArgs(TResult matchResult)
        {
            m_result = matchResult;
        }

        /// <summary>
        /// Deserialization constructor
        /// </summary>
        private MatchedEventArgs()
        {
        }

        /// <summary>
        /// Get match result
        /// </summary>
        public TResult Result
        {
            get { return m_result; }
        }
    }
}
