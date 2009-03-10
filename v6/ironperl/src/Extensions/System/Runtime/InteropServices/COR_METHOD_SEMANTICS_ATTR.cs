using System;
using System.Collections.Generic;
using System.Text;

namespace System.Runtime.InteropServices
{
    [Flags]
    public enum COR_METHOD_SEMANTICS_ATTR : uint
    {
        /// <summary>
        /// Setter for property.  
        /// </summary>
        /// <remarks>
        /// In MSIL it is a ".set" directive.
        /// </remarks>
        Setter = 0x0001,

        /// <summary>
        /// Getter for property. 
        /// </summary>
        /// <remarks>
        /// In MSIL it is a ".get" directive.
        /// </remarks>
        Getter = 0x0002,

        /// <summary>
        /// Other method for property or event.  
        /// </summary>
        ///  <remarks>
        /// In MSIL it is a ".other" directive.
        /// </remarks>
        Other = 0x0004,

        /// <summary>
        /// Add method for event.  
        /// </summary>
        ///  <remarks>
        /// In MSIL it is a ".addon" directive.
        /// </remarks>
        AddOn = 0x0008,

        /// <summary>
        /// RemoveOn method for event    
        /// </summary>
        ///  <remarks>
        /// In MSIL it is a ".removeon" directive.
        /// </remarks>
        RemoveOn = 0x0010,     // RemoveOn method for event

        /// <summary>
        /// Fire method for event.   
        /// </summary>
        ///  <remarks>
        /// In MSIL it is a ".fire" directive.
        /// </remarks>
        Fire = 0x0020
    }

    public static class COR_METHOD_SEMANTICS_ATTR_Helper
    {
        public static IntPtr AllocCorMethodSemanticsAttr(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            IntPtr semAttrPtr = System.Runtime.InteropServices.Marshal.AllocHGlobal(sizeof(COR_METHOD_SEMANTICS_ATTR));
            System.Runtime.InteropServices.Marshal.StructureToPtr(semAttr, semAttrPtr, true);
            return semAttrPtr;
        }

        public static void FreeCorMethodSemanticsAttr(IntPtr semAttrPtr)
        {
            System.Runtime.InteropServices.Marshal.FreeHGlobal(semAttrPtr);
        }

        public static bool IsMsSetter(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            return (semAttr & COR_METHOD_SEMANTICS_ATTR.Setter) != 0;
        }

        public static bool IsMsGetter(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            return (semAttr & COR_METHOD_SEMANTICS_ATTR.Getter) != 0;
        }

        public static bool IsMsOther(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            return (semAttr & COR_METHOD_SEMANTICS_ATTR.Other) != 0;
        }

        public static bool IsMsAddOn(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            return (semAttr & COR_METHOD_SEMANTICS_ATTR.AddOn) != 0;
        }

        public static bool IsMsRemoveOn(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            return (semAttr & COR_METHOD_SEMANTICS_ATTR.RemoveOn) != 0;
        }

        public static bool IsMsFire(/*this*/COR_METHOD_SEMANTICS_ATTR semAttr)
        {
            return (semAttr & COR_METHOD_SEMANTICS_ATTR.Fire) != 0;
        }
    }
}
