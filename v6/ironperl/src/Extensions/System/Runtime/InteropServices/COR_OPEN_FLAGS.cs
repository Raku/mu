using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics.SymbolStore;
using System.Runtime.InteropServices.ComTypes;

namespace System.Runtime.InteropServices
{
    public enum COR_OPEN_FLAGS : uint
    {
        ofRead = 0x00000000,     // Open scope for read
        ofWrite = 0x00000001,     // Open scope for write.
        ofReadWriteMask = 0x00000001,     // Mask for read/write bit.

        ofCopyMemory = 0x00000002,     // Open scope with memory. Ask metadata to maintain its own copy of memory.

        ofManifestMetadata = 0x00000008,     // Open scope on ngen image, return the manifest metadata instead of the IL metadata
        ofReadOnly = 0x00000010,     // Open scope for read. Will be unable to QI for a IMetadataEmit* interface
        ofTakeOwnership = 0x00000020,     // The memory was allocated with CoTaskMemAlloc and will be freed by the metadata

        // These are obsolete and are ignored.
        ofCacheImage = 0x00000004,     // EE maps but does not do relocations or verify image
        ofNoTypeLib = 0x00000080,     // Don't OpenScope on a typelib.

        // Internal bits
        ofReserved1 = 0x00000100,     // Reserved for internal use.
        ofReserved2 = 0x00000200,     // Reserved for internal use.
        ofReserved = 0xffffff40      // All the reserved bits.

    }

    public static class COR_OPEN_FLAGS_Helper
    {
        public static uint MarshalCorOpenFlags(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (uint)openFlags;
        }

        public static IntPtr AllocCorOpenFlags(/*this*/COR_OPEN_FLAGS openFlags)
        {
            IntPtr openFlagsPtr = System.Runtime.InteropServices.Marshal.AllocHGlobal(sizeof(COR_OPEN_FLAGS));
            System.Runtime.InteropServices.Marshal.StructureToPtr(openFlags, openFlagsPtr, true);
            return openFlagsPtr;
        }

        public static void FreeCorOpenFlags(IntPtr openFlagsPtr)
        {
            System.Runtime.InteropServices.Marshal.FreeHGlobal(openFlagsPtr);
        }

        public static bool IsOfRead(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofReadWriteMask) == COR_OPEN_FLAGS.ofRead;
        }

        public static bool IsOfReadWrite(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofReadWriteMask) == COR_OPEN_FLAGS.ofWrite;
        }

        public static bool IsOfCopyMemory(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofCopyMemory) != 0;
        }

        public static bool IsOfManifestMetadata(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofManifestMetadata) != 0;
        }

        public static bool IsOfReadOnly(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofReadOnly) != 0;
        }

        public static bool IsOfTakeOwnership(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofTakeOwnership) != 0;
        }

        public static bool IsOfReserved(/*this*/COR_OPEN_FLAGS openFlags)
        {
            return (openFlags & COR_OPEN_FLAGS.ofReserved) != 0;
        }
    }
}
