using System.Collections.Generic;
using System.Extensions;

namespace System.Windows.Cli
{
    using Serializable = System.Runtime.Serialization.Serializable;

    /// <summary>
    /// Represents command parsing result
    /// </summary>
    [Serializable]
    sealed class CommandParsingResult: Serializable, ICommandParsingResult
    {
        private readonly string m_commandName;
        private readonly string[] m_commandArgs;

        /// <summary>
        /// Create parsing result.
        /// </summary>
        /// <param name="name">Command name</param>
        /// <param name="args">Command arguments</param>
        public CommandParsingResult(string name, string[] args)
        {
            ExceptionManager.CheckOnNull(name, "name");
            ExceptionManager.CheckOnNull(args, "args");
            m_commandName = name;
            m_commandArgs = args;
        }

        /// <summary>
        /// Get command argument by index
        /// </summary>
        /// <param name="index">Index of the command argument</param>
        /// <returns>Value of the argument</returns>
        public string this[int index]
        {
            get { return m_commandArgs[index]; }
        }

        /// <summary>
        /// Get command name
        /// </summary>
        public string Name
        {
            get { return m_commandName; }
        }

        #region IEnumerable<string> Members

        /// <summary>
        /// Get all command arguments
        /// </summary>
        /// <returns></returns>
        public IEnumerator<string> GetEnumerator()
        {
            return m_commandArgs.UnsafeCast<IEnumerable<string>>().GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion
    }
}
