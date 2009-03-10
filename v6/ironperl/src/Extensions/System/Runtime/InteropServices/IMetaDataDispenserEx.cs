using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    [ComImport]
    [Guid(RCWHelper.IID_IMetaDataDispenserEx)]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    [CoClass(typeof(CoMetaDataDispenserClass))]
    internal interface IMetaDataDispenserEx
    {
        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT DefineScope(
            [In, MarshalAs(UnmanagedType.Struct)] ref Guid rclsid,
            [In, MarshalAs(UnmanagedType.U4)]  COR_OPEN_FLAGS dwCreateFlags,
            [In, MarshalAs(UnmanagedType.Struct)] ref Guid riid,
            out IntPtr ppIUnk
            );

        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT OpenScope(
            [In, MarshalAs(UnmanagedType.LPWStr)]  string file,
            [In, MarshalAs(UnmanagedType.U4)]  COR_OPEN_FLAGS dwOpenFlags,
            [In, MarshalAs(UnmanagedType.Struct)] ref Guid riid,
            out IntPtr ppIUnk
        );

        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT OpenScopeOnMemory(
            [In]  IntPtr pData,
            [In, MarshalAs(UnmanagedType.U4)]  UInt32 cbData,
            [In, MarshalAs(UnmanagedType.U4)]  COR_OPEN_FLAGS dwOpenFlags,
            [In, MarshalAs(UnmanagedType.Struct)] ref  Guid riid,
            out IntPtr ppIUnk
        );

        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT GetOption(
            [In, MarshalAs(UnmanagedType.Struct)] ref  Guid optionId,
            [MarshalAs(UnmanagedType.Struct)] out ValueType pvalue //VARIANT
        );

        //[MethodImpl(MethodImplOptions.Unmanaged)]
        [PreserveSig]
        HRESULT SetOption(
            [In, MarshalAs(UnmanagedType.Struct)] ref  Guid optionId,
            [In, MarshalAs(UnmanagedType.Struct)] ValueType pvalue
        );
    }
}
