using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace System.Runtime.InteropServices
{
    // Coclass for IMetaDataDispenserEx interface.  
    [ComImport]
    [Guid(RCWHelper.CLSID_CorMetaDataDispenser)]
    class CoMetaDataDispenserClass
    {
        //constructor of this class is runtime controlled, not CIL controlled 
    }
}
