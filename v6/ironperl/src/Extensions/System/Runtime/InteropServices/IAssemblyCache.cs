using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    /// <summary>
    /// Represents a GAC manager
    /// </summary>
    [ComImport]
    [GuidAttribute(RCWHelper.IID_IAssemblyCache)]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    internal interface IAssemblyCache
    {
        /// <summary>
        /// Unistall assembly from GAC
        /// </summary>
        /// <param name="flags">Uninstall flags</param>
        /// <param name="assemblyName"></param>
        /// <param name="reserved"></param>
        /// <param name="disposition"></param>
        /// <returns></returns>
        [PreserveSig]
        HRESULT UninstallAssembly(uint flags, string assemblyName, IntPtr reserved, ref uint disposition);
        
        /// <summary>
        /// Get assembly information
        /// </summary>
        /// <param name="flags"></param>
        /// <param name="assemblyName"></param>
        /// <param name="assemblyInfo"></param>
        /// <returns></returns>
        [PreserveSig]
        HRESULT QueryAssemblyInfo([MarshalAs(UnmanagedType.I4)]ASM_INFO_FLAG flags, string assemblyName, ref ASM_CACHE_ITEM_INFO assemblyInfo);
        
        [PreserveSig]
        HRESULT CreateAssemblyCacheItem(uint flags, 
            IntPtr pvReserved, 
            ref IntPtr ppAsmItem, 
            string assemblyName);

        [PreserveSig]
        HRESULT CreateAssemblyScavenger(ref object assemblyScavenger);

        [PreserveSig]
        HRESULT InstallAssembly(uint flags, string manifestFilePath, IntPtr reserved);
    }
}
