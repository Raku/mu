using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace System.Runtime.InteropServices
{
    /// <summary>
    /// Represents Runtime Callable Wrapper manager
    /// </summary>
    static class RCWHelper
    {
        public const string CLSID_CorMetaDataDispenser = "E5CB7A31-7512-11D2-89CE-0080C792E5D8";
        public const string IID_IMetaDataDispenser = "B81FF171-20F3-11D2-8DCC-00A0C9B09C19";
        public const string IID_IMetaDataDispenserEx = "31BCFCE2-DAFB-11D2-9F81-00C04F79A0A3";
        public const string IID_IMetaDataImport = "7DAC8207-D3AE-4C75-9B67-92801A497D44";
        public const string IID_IAssemblyName = "CD193BC0-B4BC-11d2-9833-00C04FC31D2E";
        public const string IID_IAssemblyEnum = "21B8916C-F28E-11D2-A473-00C04F8EF448";
        public const string IID_IAssemblyCache = "E707DCDE-D1CD-11D2-BAB9-00C04F8ECEAE";

        [DllImport("ole32.dll")]
        [Obsolete("Use managed CreateCoClass method instead of pinvoke unmanaged method")]
        public static extern int CoCreateInstance([In] ref Guid rclsid,
                                                   [In, MarshalAs(UnmanagedType.IUnknown)] Object pUnkOuter,
                                                   [In] uint dwClsContext,
                                                   [In] ref Guid riid,
                                                   [Out, MarshalAs(UnmanagedType.Interface)] out Object ppv);


        public static object CreateCoClass(Guid clsid)
        {
            Type coClass = Type.GetTypeFromCLSID(clsid, true);
            return Activator.CreateInstance(coClass);
        }

        public static T GetInterfaceFromIUnknown<T>(IntPtr pIUnknown)
        {
            return (T)Marshal.GetTypedObjectForIUnknown(pIUnknown, typeof(T));
        }
    }
}
