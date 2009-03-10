using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    /// <summary>
    /// Represents a Fusion assembly name resolver
    /// </summary>
    [ComImport]
    [GuidAttribute(RCWHelper.IID_IAssemblyName)]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    internal interface IAssemblyName
    {
        [PreserveSig]
        HRESULT SetProperty(uint propertyId, IntPtr value, uint size);

        [PreserveSig]
        HRESULT GetProperty(uint propertyId, IntPtr value, ref uint size);

        [PreserveSig]
        HRESULT Finalize();

        [PreserveSig]
        HRESULT GetDisplayName(
                [MarshalAs(UnmanagedType.LPWStr), Out] StringBuilder szDisplayName,
                [In, Out] ref uint pccDisplayName,
                [MarshalAs(UnmanagedType.I4)] ASM_NAME_DISPLAY_FLAGS dwDisplayFlags);

        HRESULT Reserved(
            [In] ref Guid refIID,
            [In] IntPtr pUnkReserved1,
            [In] IntPtr pUnkReserved2,
            [In, MarshalAs(UnmanagedType.LPStr)] string szReserved,
            [In, MarshalAs(UnmanagedType.I8)]long llReserved,
            [In] IntPtr pvReserved,
            [In, MarshalAs(UnmanagedType.U4), ComAliasName("DWORD")] UInt32 cbReserved,
            out IntPtr ppReserved
            );

        HRESULT GetName([In, Out, MarshalAs(UnmanagedType.U4), ComAliasName("LPDWORD")] uint lpcwBuffer,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 0)]out char[] pwzName);

        /*[PreserveSig]
        HRESULT BindToObject(Object interfaceId, Object bindSink, Object applicationContext, string codeBase, long flags, int reserved, uint reservedSize, ref int pointer);*/
        
        /*[PreserveSig]
        HRESULT GetName(ref uint buffer, ref int name);*/
        
        [PreserveSig]
        HRESULT GetVersion(out uint pdwVersionHi, out uint pdwVersionLow);
        
        [PreserveSig]
        HRESULT IsEqual(IAssemblyName pName, uint dwCmpFlags);
        
        [PreserveSig]
        HRESULT Clone(ref IAssemblyName ppName);
    }
}
