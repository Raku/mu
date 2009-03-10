using System;
using System.Extensions;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;

namespace System.Windows.Cli
{
    public class ConsoleApplication : Component
    {
        private readonly CommandLine m_commandLine;

        public ConsoleApplication()
        {
            m_commandLine = new CommandLine();
        }

        protected void InitializeComponents(IContainer container)
        {
            foreach (var component in container.Components)
                if (component is ICommand)
                    m_commandLine.Commands.Add(component as ICommand);
        }

        /// <summary>
        /// Getr command line manager
        /// </summary>
        public CommandLine CommandManager
        {
            get { return m_commandLine; }
        }
    }
}
