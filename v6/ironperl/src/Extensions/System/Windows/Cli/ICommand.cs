using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections.Processing;

namespace System.Windows.Cli
{
    /// <summary>
    /// Represents an ultimate class for building console commands
    /// </summary>
    public interface ICommand: IPattern<char, ICommandParsingResult>
    {
        /// <summary>
        /// Get value indicating that the current command has
        /// the fixed position in the command line
        /// </summary>
        bool Ordered
        {
            get;
        }

        /// <summary>
        /// Get position of the current command
        /// </summary>
        /// <exception cref="System.NotSupportedException">The current command
        /// is not position-dependent. If Ordered property is false then
        /// the current exceptions will raised</exception>
        int Position
        {
            get;
        }

        /// <summary>
        /// Get decription of the current command, which can be
        /// displayed in help command
        /// </summary>
        string Description
        {
            get;
        }
    }
}
