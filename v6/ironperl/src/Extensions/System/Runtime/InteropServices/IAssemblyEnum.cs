using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    /// <summary>
    /// Represents a COM-component for GAC assembly enumeration
    /// </summary>
    [ComImport]
    [Guid(RCWHelper.IID_IAssemblyEnum)]
    [InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
    internal interface IAssemblyEnum
    {
        /// <summary>
        /// Get next assembly in the GAC
        /// </summary>
        /// <param name="pvReserved">Rserved</param>
        /// <param name="ppName"></param>
        /// <param name="dwFlags"></param>
        /// <returns></returns>
        [PreserveSig]
        HRESULT GetNextAssembly(IntPtr pvReserved,
                                out IAssemblyName ppName,
                                uint dwFlags);

        /// <summary>
        /// Reser assembly iterator
        /// </summary>
        /// <returns></returns>
        [PreserveSig]
        HRESULT Reset();

        /// <summary>
        /// Create clonet of the current COM-wrapper
        /// </summary>
        /// <param name="enumerator"></param>
        /// <returns></returns>
        [PreserveSig]
        HRESULT Clone(out IAssemblyEnum enumerator);
    }
}
