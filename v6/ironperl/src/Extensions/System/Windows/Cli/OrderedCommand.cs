using System.Collections.Processing;
using System.ComponentModel;
using System.Extensions;
using System.Text;

namespace System.Windows.Cli
{
    using ExceptionManager = System.Extensions.ExceptionManager;

    /// <summary>
    /// Represents ordered command
    /// </summary>
    [ToolboxItemFilter("Command Line Interface", ToolboxItemFilterType.Require)]
    [DisplayName("Ordered Command")]
    [Description("Ordered Command")]
    [DesignTimeVisible(true)]
    public class OrderedCommand: Component, ICommand
    {
        private int m_position;

        public OrderedCommand()
            : this(null)
        {
        }

        /// <summary>
        /// Create instance of the current console command at design-time
        /// </summary>
        /// <param name="container"></param>
        public OrderedCommand(IContainer container)
        {
            if (container.IsNotNull())
                container.Add(this);
        }

        #region ICommand Members

        bool ICommand.Ordered
        {
            get { return true; }
        }

        /// <summary>
        /// Get or set position of the command
        /// </summary>
        [Browsable(true)]
        [Description("Get or set position of the command")]
        public int Position
        {
            get { return m_position; }
            set
            {
                if (value < 0) return;
                m_position = value;
            }
        }

        /// <summary>
        /// Get or set help string for the current command
        /// </summary>
        [Browsable(true)]
        [Description("Get or set help string for the current command")]
        public string Description
        {
            get;
            set;
        }

        /// <summary>
        /// Get or set parser for the current command
        /// </summary>
        [Browsable(false)]
        public IPattern<char, string> Parser
        {
            get;
            set;
        }

        #endregion

        #region IPattern<char,ICommandParsingResult> Members

        ICommandParsingResult IPattern<char,ICommandParsingResult>.Match(INavigationContext<char> navigator)
        {
            ExceptionManager.CheckOnNull(navigator, "navigator");
            if (Parser.IsNotNull())
                return new CommandParsingResult("", Parser.Match(navigator).AsArray());
            var result = new StringBuilder();
            var currentPos = navigator.CurrentPosition;
            while (currentPos != null)
            {
                var character = navigator.GetValueAtMarker(currentPos);
                switch (character)
                {
                    case '"':
                        return OnMatched(ParseSpacedCommand(navigator));
                    case ' ':
                        return result.Length.IsZero() ?
                            null : OnMatched(result.ToString());
                    default: result.Append(character); break;
                }
                currentPos = navigator.MoveNext();
            }
            return result.Length.IsZero() ?
                   null : OnMatched(result.ToString());
        }

        public event EventHandler<MatchedEventArgs<ICommandParsingResult>> Matched;

        #endregion

        private string ParseSpacedCommand(INavigationContext<char> navigator)
        {
            var currentPos = navigator.MoveNext();  //Pass the " character
            var result = new StringBuilder();
            while (currentPos != null)
            {
                var character = navigator.GetValueAtMarker(currentPos);
                if (character == '"')
                {
                    navigator.MoveNext();   //Pass " character
                    break;
                }
                result.Append(character);
                currentPos = navigator.MoveNext();
            }
            return result.ToString();
        }

        private CommandParsingResult OnMatched(string result)
        {
            var cmdResult = new CommandParsingResult("", result.AsArray());
            if (Matched.IsNotNull())
                Matched(this, new MatchedEventArgs<ICommandParsingResult>(cmdResult));
            return cmdResult;
        }
    }
}
