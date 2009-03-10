using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

namespace System.Runtime.InteropServices
{
    [Flags]
    [Serializable]
    public enum COR_PINVOKE_MAP : ushort
    {
        /// <summary>
        /// Pinvoke is to use the member name as specified.
        /// </summary>
        NoMangle = 0x0001,

        /// <summary>
        /// Use this mask to retrieve the CharSet information.
        /// </summary>
        CharSetMask = 0x0006,

        /// <summary>
        /// Not specified
        /// </summary>
        CharSetNotSpec = 0x0000,

        /// <summary>
        /// Single byte  - ANSI chars
        /// </summary>
        CharSetAnsi = 0x0002,

        /// <summary>
        /// Unicode - UTF 16 chars
        /// </summary>
        CharSetUnicode = 0x0004,

        /// <summary>
        /// Platform depended
        /// </summary>
        CharSetAuto = 0x0006,

        /// <summary>
        /// ???
        /// </summary>
        BestFitUseAssem = 0x0000,

        /// <summary>
        /// ???
        /// </summary>
        BestFitEnabled = 0x0010,

        /// <summary>
        /// ???
        /// </summary>
        BestFitDisabled = 0x0020,

        /// <summary>
        /// ???
        /// </summary>
        BestFitMask = 0x0030,

        /// <summary>
        /// ???
        /// </summary>
        ThrowOnUnmappableCharUseAssem = 0x0000,

        /// <summary>
        /// ???
        /// </summary>
        ThrowOnUnmappableCharEnabled = 0x1000,

        /// <summary>
        /// ???
        /// </summary>
        ThrowOnUnmappableCharDisabled = 0x2000,

        /// <summary>
        /// ???
        /// </summary>
        ThrowOnUnmappableCharMask = 0x3000,

        /// <summary>
        /// Information about target function. Not relevant for fields.
        /// </summary>
        SupportsLastError = 0x0040,

        /// <summary>
        /// None of the calling convention flags is relevant for fields. 
        /// </summary>
        CallConvMask = 0x0700,

        /// <summary>
        /// Pinvoke will use native callconv appropriate to target windows platform.
        /// </summary>
        CallConvWinapi = 0x0100,

        /// <summary>
        /// Cdecl calling conventions.
        /// </summary>
        CallConvCdecl = 0x0200,

        /// <summary>
        /// Stdcall  calling conventions.
        /// </summary>
        CallConvStdcall = 0x0300,

        /// <summary>
        /// In M9, pinvoke will raise exception.
        /// </summary>
        CallConvThiscall = 0x0400,

        /// <summary>
        /// _fastcall calling conventions.
        /// </summary>
        CallConvFastcall = 0x0500
    };

    public static class COR_PINVOKE_MAP_Helper
    {
        public static IntPtr AllocCorPinvokeMapH(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            IntPtr pinvokeMapPtr = System.Runtime.InteropServices.Marshal.AllocHGlobal(sizeof(COR_PINVOKE_MAP));
            System.Runtime.InteropServices.Marshal.StructureToPtr(pinvokeMap, pinvokeMapPtr, true);
            return pinvokeMapPtr;
        }

        public static void FreeCorPinvokeMap(IntPtr pinvokeMapPtr)
        {
            System.Runtime.InteropServices.Marshal.FreeHGlobal(pinvokeMapPtr);
        }

        public static bool IsPmNoMangle(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.NoMangle) != 0;
        }

        public static bool IsPmCharSetNotSpec(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CharSetMask) == COR_PINVOKE_MAP.CharSetNotSpec;
        }

        public static bool IsPmCharSetAnsi(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CharSetMask) == COR_PINVOKE_MAP.CharSetAnsi;
        }

        public static bool IsPmCharSetUnicode(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CharSetMask) == COR_PINVOKE_MAP.CharSetUnicode;
        }

        public static bool IsPmCharSetAuto(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CharSetMask) == COR_PINVOKE_MAP.CharSetAuto;
        }

        public static bool IsPmSupportsLastError(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.SupportsLastError) != 0;
        }

        public static bool IsPmCallConvWinapi(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CallConvMask) == COR_PINVOKE_MAP.CallConvWinapi;
        }

        public static bool IsPmCallConvCdecl(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CallConvMask) == COR_PINVOKE_MAP.CallConvCdecl;
        }

        public static bool IsPmCallConvStdcall(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CallConvMask) == COR_PINVOKE_MAP.CallConvStdcall;
        }

        public static bool IsPmCallConvThiscall(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CallConvMask) == COR_PINVOKE_MAP.CallConvThiscall;
        }

        public static bool IsPmCallConvFastcall(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.CallConvMask) == COR_PINVOKE_MAP.CallConvFastcall;
        }

        public static bool IsPmBestFitEnabled(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.BestFitMask) == COR_PINVOKE_MAP.BestFitEnabled;
        }

        public static bool IsPmBestFitDisabled(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.BestFitMask) == COR_PINVOKE_MAP.BestFitDisabled;
        }

        public static bool IsPmBestFitUseAssem(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.BestFitMask) == COR_PINVOKE_MAP.BestFitUseAssem;
        }

        public static bool IsPmThrowOnUnmappableCharEnabled(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.ThrowOnUnmappableCharMask) == COR_PINVOKE_MAP.ThrowOnUnmappableCharEnabled;
        }

        public static bool IsPmThrowOnUnmappableCharDisabled(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.ThrowOnUnmappableCharMask) == COR_PINVOKE_MAP.ThrowOnUnmappableCharDisabled;
        }

        public static bool IsPmThrowOnUnmappableCharUseAssem(/*this*/COR_PINVOKE_MAP pinvokeMap)
        {
            return (pinvokeMap & COR_PINVOKE_MAP.ThrowOnUnmappableCharMask) == COR_PINVOKE_MAP.ThrowOnUnmappableCharUseAssem;
        }
    }
}
