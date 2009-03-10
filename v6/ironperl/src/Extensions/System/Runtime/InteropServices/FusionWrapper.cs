using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    /// <summary>
    /// Represents a wrapper for the unmanaged Fusion API
    /// </summary>
    
    unsafe class FusionWrapper : IAssemblyName, IAssemblyEnum, IDisposable
    {
        private readonly IAssemblyName m_assemblyName;
        private readonly IAssemblyEnum m_assemblyEnum;

        public FusionWrapper()
        {
            int hr = CreateAssemblyNameObject(out m_assemblyName,
                null, //Enum all GAC assemblies
                CREATE_ASM_NAME_FLAGS.CANOF_PARSE_DISPLAY_NAME,
                IntPtr.Zero);
            if (hr < 0)
                Marshal.ThrowExceptionForHR(hr);
            hr = CreateAssemblyEnum(out m_assemblyEnum,
                IntPtr.Zero,
                m_assemblyName,
                2,
                IntPtr.Zero);
        }

        [DllImport("fusion.dll", ExactSpelling = true, CharSet = CharSet.Auto), PreserveSig]
        [CompilerServices.Unsafe]
        internal static extern int CreateAssemblyEnum(out IAssemblyEnum ppEnum,
                                                        IntPtr pAppCtx,
                                                        IAssemblyName pName,
                                                        uint dwFlags,
                                                        IntPtr pvReserved);

        internal static IAssemblyEnum CreateAssemblyEnumWithFilter(IAssemblyName pName)
        {
            IAssemblyEnum result = null;
            int hr = CreateAssemblyEnum(out result,
                IntPtr.Zero,
                pName,
                2,
                IntPtr.Zero);
            if (hr < 0) Marshal.ThrowExceptionForHR(hr);
            return result;
        }

        internal static IAssemblyEnum CreateAssemblyEnum()
        {
            return CreateAssemblyEnumWithFilter(null);
        }

        [DllImport("fusion.dll", ExactSpelling = true, CharSet = CharSet.Auto), PreserveSig]
        internal static extern int CreateAssemblyCache(out IAssemblyCache ppAsmCache,
                                                        uint dwReserved);

        internal static IAssemblyCache CreateAssemblyCache()
        {
            IAssemblyCache result = null;
            int hr = CreateAssemblyCache(out result, 0);
            if (hr < 0) Marshal.ThrowExceptionForHR(hr);
            return result;
        }

        [DllImport("fusion.dll")]
        internal static extern int CreateAssemblyNameObject(
                out IAssemblyName ppAssemblyNameObj,
                [MarshalAs(UnmanagedType.LPWStr)] string szAssemblyName,
                CREATE_ASM_NAME_FLAGS flags,
                IntPtr pvReserved);

        internal static IAssemblyName CreateAssemblyNameObject(string szAssemblyName)
        {
            IAssemblyName result = null;
            int hr = CreateAssemblyNameObject(out result, szAssemblyName,
                CREATE_ASM_NAME_FLAGS.CANOF_PARSE_DISPLAY_NAME,
                IntPtr.Zero);
            if (hr < 0) Marshal.ThrowExceptionForHR(hr);
            return result;
        }

        #region IAssemblyName
        [PreserveSig]
        public HRESULT SetProperty(uint propertyId, IntPtr value, uint size)
        {
            return m_assemblyName.SetProperty(propertyId, value, size);
        }

        [PreserveSig]
        public HRESULT GetProperty(uint propertyId, IntPtr value, ref uint size)
        {
            return m_assemblyName.GetProperty(propertyId, value, ref size);
        }

        [PreserveSig]
        HRESULT IAssemblyName.Finalize()
        {
            return m_assemblyName.Finalize();
        }

        [PreserveSig]
        public HRESULT GetDisplayName(
                [MarshalAs(UnmanagedType.LPWStr), Out] StringBuilder szDisplayName,
                [In, Out] ref uint pccDisplayName,
                [MarshalAs(UnmanagedType.I4)] ASM_NAME_DISPLAY_FLAGS dwDisplayFlags)
        {
            return m_assemblyName.GetDisplayName(szDisplayName,
                ref pccDisplayName,
                dwDisplayFlags);
        }

        public HRESULT Reserved(
            [In] ref Guid refIID,
            [In] IntPtr pUnkReserved1,
            [In] IntPtr pUnkReserved2,
            [In, MarshalAs(UnmanagedType.LPStr)] string szReserved,
            [In, MarshalAs(UnmanagedType.I8)]long llReserved,
            [In] IntPtr pvReserved,
            [In, MarshalAs(UnmanagedType.U4), ComAliasName("DWORD")] UInt32 cbReserved,
            out IntPtr ppReserved
            )
        {
            return m_assemblyName.Reserved(ref refIID,
                pUnkReserved1,
                pUnkReserved2,
                szReserved,
                llReserved,
                pvReserved,
                cbReserved,
                out ppReserved);
        }

        public HRESULT GetName([In, Out, MarshalAs(UnmanagedType.U4), ComAliasName("LPDWORD")] uint lpcwBuffer,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 0)]out char[] pwzName)
        {
            return m_assemblyName.GetName(lpcwBuffer, out pwzName);
        }

        [PreserveSig]
        public HRESULT GetVersion(out uint pdwVersionHi, out uint pdwVersionLow)
        {
            return m_assemblyName.GetVersion(out pdwVersionHi, out pdwVersionLow);
        }

        [PreserveSig]
        public HRESULT IsEqual(IAssemblyName pName, uint dwCmpFlags)
        {
            return m_assemblyName.IsEqual(pName, dwCmpFlags);
        }

        [PreserveSig]
        public HRESULT Clone(ref IAssemblyName ppName)
        {
            return m_assemblyName.Clone(ref ppName);
        }
        #endregion

        #region IAssemblyEnum Members
        [PreserveSig]
        public HRESULT GetNextAssembly(IntPtr pvReserved,
                                out IAssemblyName ppName,
                                uint dwFlags)
        {
            return m_assemblyEnum.GetNextAssembly(pvReserved,
                out ppName,
                dwFlags);
        }

        [PreserveSig]
        public HRESULT Reset()
        {
            return m_assemblyEnum.Reset();
        }

        [PreserveSig]
        public HRESULT Clone(out IAssemblyEnum enumerator)
        {
            return m_assemblyEnum.Clone(out enumerator);
        }
        #endregion

        #region IDisposable Members
        public void Dispose()
        {
            Marshal.ReleaseComObject(m_assemblyName);
            Marshal.ReleaseComObject(m_assemblyEnum);
        }
        #endregion
    }
}
