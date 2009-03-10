using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    /// <summary>
    /// Represents assembly display name parts
    /// </summary>
    [Flags]
    enum ASM_NAME_DISPLAY_FLAGS : int
    {
        VERSION = 0x01,
        CULTURE = 0x02,
        PUBLIC_KEY_TOKEN = 0x04,
        PROCESSORARCHITECTURE = 0x20,
        RETARGETABLE = 0x80,
        ALL = VERSION
                        | CULTURE
                        | PUBLIC_KEY_TOKEN
                        | PROCESSORARCHITECTURE
                        | RETARGETABLE
    }
}
