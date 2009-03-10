using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    /// <summary>
    /// Represents assembly information retreiving flags
    /// </summary>
    [Flags]
    enum ASM_INFO_FLAG : uint
    {
        None = 0,
        Validate = 1,
        GetSize = 2
    }
}
