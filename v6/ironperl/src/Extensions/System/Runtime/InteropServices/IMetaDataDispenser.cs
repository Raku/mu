using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices.ComTypes;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    [Guid(RCWHelper.IID_IMetaDataDispenser)]
    [ComImport]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    [Obsolete("IMetaDataDispenser now is obsolete. Use IMetaDataDispenserEx")]
    internal interface IMetaDataDispenser
    {
        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT DefineScope(
            [In, MarshalAs(UnmanagedType.Struct)] ref Guid rclsid,
            [In, MarshalAs(UnmanagedType.U4), ComAliasName("DWORD")]  COR_OPEN_FLAGS dwCreateFlags,
            [In, MarshalAs(UnmanagedType.Struct)] ref Guid riid,
            [ComAliasName("IUnknown**")] out IntPtr ppIUnk
            );

        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT OpenScope(
            [In, MarshalAs(UnmanagedType.LPWStr), ComAliasName("LPCWSTR")]  string file,
            [In, MarshalAs(UnmanagedType.U4), ComAliasName("DWORD")]  COR_OPEN_FLAGS dwOpenFlags,
            [In, MarshalAs(UnmanagedType.Struct)] ref Guid riid,
            [ComAliasName("IUnknown**")] out IntPtr ppIUnk
        );

        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT OpenScopeOnMemory(
            [In, ComAliasName("LPCVOID")]  IntPtr pData,
            [In, MarshalAs(UnmanagedType.U4), ComAliasName("ULONG")]  UInt32 cbData,
            [In, MarshalAs(UnmanagedType.U4), ComAliasName("DWORD")]  COR_OPEN_FLAGS dwOpenFlags,
            [In, MarshalAs(UnmanagedType.Struct)] ref  Guid riid,
            [ComAliasName("IUnknown**")] out IntPtr ppIUnk
        );
    }
}
