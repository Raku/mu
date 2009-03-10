using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    /// <summary>
    /// Represents unmanaged information about GAC assembly
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    struct ASM_CACHE_ITEM_INFO
    {
        public uint cbAssemblyInfo;
        public uint dwAssemblyFlags;
        public ulong uliAssemblySizeInKB;

        [MarshalAs(UnmanagedType.LPWStr)]
        public string pszCurrentAssemblyPathBuf;
        public uint cchBuf;
    }
}
