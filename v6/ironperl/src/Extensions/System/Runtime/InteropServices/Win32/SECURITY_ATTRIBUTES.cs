using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Runtime.InteropServices.Win32
{
    /// <summary>
    /// Represents Windows NT security descriptor
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    struct SECURITY_ATTRIBUTES
    {
        /// <summary>
        /// The size, in bytes, of this structure. 
        /// Set this value to the size of the SECURITY_ATTRIBUTES structure.
        /// </summary>
        public int nLength;

        /// <summary>
        /// A pointer to a security descriptor for the object that controls the sharing of it. 
        /// If IntPtr.Zero is specified for this member, the object is assigned the default security descriptor of the calling process.
        /// </summary>
        public IntPtr lpSecurityDescriptor;

        /// <summary>
        /// A Boolean value that specifies whether the returned handle 
        /// is inherited when a new process is created. 
        /// If this member is TRUE, the new process inherits the handle.
        /// </summary>
        public bool bInheritHandle;
    }
}
