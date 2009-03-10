using System;
using System.Collections.Generic;
using System.Collections.Generic.Extensions;
using System.Linq;
using System.Text;
using System.ComponentModel;
using System.Extensions;
using System.Collections.Processing;

namespace System.Windows.Cli
{
    /// <summary>
    /// Represents command line parser
    /// </summary>
    public class CommandLine: IPattern<char, ICommandParsingResult[]>
    {
        private string m_commandLine;
        private readonly IList<ICommand> m_commands;

        public CommandLine(string cmdLine)
        {
            m_commandLine = cmdLine;
            m_commands = new List<ICommand>();
        }

        public CommandLine()
            : this(Environment.CommandLine)
        {
        }

        /// <summary>
        /// Get commands
        /// </summary>
        public IList<ICommand> Commands
        {
            get { return m_commands; }
        }

        /// <summary>
        /// Get or set command line
        /// </summary>
        public string CommandLineString
        {
            get { return m_commandLine; }
            set { if (!value.IsNullOrEmpty()) m_commandLine = value; }
        }

        /// <summary>
        /// Get command line string
        /// </summary>
        /// <returns>Command line string</returns>
        public override string ToString()
        {
            return m_commandLine;
        }

        #region IPattern<char,ICommandParsingResult[]> Members

        ICommandParsingResult[] IPattern<char,ICommandParsingResult[]>.Match(INavigationContext<char> context)
        {
            return Match(context);
        }

        public event EventHandler<MatchedEventArgs<ICommandParsingResult[]>> Matched;

        #endregion

        public event EventHandler<MatchedEventArgs<ICommandParsingResult>> CommandMatched;

        private ICommandParsingResult[] OnMatched(ICommandParsingResult[] result)
        {
            if (Matched != null)
                Matched(this, new MatchedEventArgs<ICommandParsingResult[]>(result));
            return result;
        }

        protected virtual ICommandParsingResult[] Match(INavigationContext<char> context)
        {
            ExceptionManager.CheckOnNull(context, "context");
            //Parse program name
            ICommand programName = new OrderedCommand();
            programName.Match(context);
            DeleteSpaces(context);
            //Now select ordered commands
            var result = new List<ICommandParsingResult>();
            var orderedCommands = from cmd in Commands
                                  where cmd.Ordered
                                  orderby cmd.Position ascending
                                  select cmd;
            //Parse commands
            foreach (var cmd in orderedCommands)
            {
                cmd.Matched += CommandMatched;
                var currentPos = context.CurrentPosition;
                var parsingResult = cmd.Match(context);
                if (parsingResult.IsNotNull() && !currentPos.Equals(context.CurrentPosition))
                    result.Add(parsingResult);
                DeleteSpaces(context);
                cmd.Matched -= CommandMatched;
            }
            //Select non-ordered commands
            var otherCommands = from cmd in Commands
                                where !cmd.Ordered
                                select cmd;
            foreach (var cmd in otherCommands)
            {
                cmd.Matched += CommandMatched;
                var currentPos = context.CurrentPosition;
                var parsingResult = cmd.Match(context);
                if (parsingResult.IsNotNull() && currentPos != context.CurrentPosition)
                    result.Add(parsingResult);
                DeleteSpaces(context);
                cmd.Matched -= CommandMatched;
            }
            return OnMatched(result.ToArray());
        }

        /// <summary>
        /// Provides matching on the current command line arguments
        /// </summary>
        /// <returns></returns>
        public ICommandParsingResult[] Match()
        {
            return Match(CommandLineString.AsNavigator());
        }

        //
        //Delete spaces from the current character sequence
        //
        private static void DeleteSpaces(INavigationContext<char> navigator)
        {
            var currentPos = navigator.CurrentPosition;
            while (currentPos != null)
            {
                var character = navigator.GetValueAtMarker(currentPos);
                if (character != ' ') break;
                currentPos = navigator.MoveNext();
            }
        }
    }
}
