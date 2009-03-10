using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Reflection
{
    [Serializable]
    public delegate T MetadataParser<T>(T member) where T :MemberInfo;
}
