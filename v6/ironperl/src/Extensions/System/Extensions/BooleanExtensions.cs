using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Extensions;

namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.Boolean class
    /// </summary>
    [TypeExtender(typeof(Boolean))]
    public static class BooleanExtensions
    {
       
        public static bool And(this bool value, bool op2)
        {
            return op2 && value;
        }

        
        public static bool AndNot(this bool value, bool op2)
        {
            return value.And(!op2);
        }

       
        public static bool And(this bool value, Condition cond)
        {
            ExceptionManager.CheckOnNull(cond, "cond");
            return value.And(cond());
        }

        
        public static bool Or(this bool value, bool op2)
        {
            return value || op2;
        }

        public static bool OrNot(this bool value, bool op2)
        {
            return value.Or(!op2);
        }
    }
}
