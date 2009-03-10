using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Windows.Cli
{
    /// <summary>
    /// Represents an ultimate interface for building command
    /// matching result
    /// </summary>
    public interface ICommandParsingResult: IEnumerable<string>
    {
        /// <summary>
        /// Get command name
        /// </summary>
        string Name
        {
            get;
        }

        /// <summary>
        /// Get command argument by index
        /// </summary>
        /// <param name="index">Index of the command argument</param>
        /// <returns>Value of the argument</returns>
        string this[int index]
        {
            get;
        }
    }
}
