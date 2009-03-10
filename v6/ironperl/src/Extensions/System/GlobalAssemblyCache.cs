using System.Collections.Generic;
using System.Collections.Generic.Extensions;
using System.Text;
using System.Reflection;
using System.Linq;
using System.Runtime.InteropServices;
using System.Extensions;

namespace System
{

    /// <summary>
    /// Represents a GAC manager
    /// </summary>
    [Runtime.CompilerServices.Singleton]
    public class GlobalAssemblyCache : ICollection<AssemblyName>
    {
        #region Static Members
        const uint IASSEMBLYCACHE_UNINSTALL_DISPOSITION_UNINSTALLED = 0x00000001;
        const uint IASSEMBLYCACHE_UNINSTALL_DISPOSITION_STILL_IN_USE = 0x00000002;
        const uint IASSEMBLYCACHE_UNINSTALL_DISPOSITION_ALREADY_UNINSTALLED = 0x00000003;
        const uint IASSEMBLYCACHE_UNINSTALL_DISPOSITION_DELETE_PENDING = 0x00000004;
        const uint IASSEMBLYCACHE_UNINSTALL_DISPOSITION_HAS_INSTALL_REFERENCES = 0x00000005;
        const uint IASSEMBLYCACHE_UNINSTALL_DISPOSITION_REFERENCE_NOT_FOUND = 0x00000006;

        private static GlobalAssemblyCache m_instance;

        /// <summary>
        /// Get Global Assembly Cache manager
        /// </summary>
        public static GlobalAssemblyCache Value
        {
            get 
            {
                if (m_instance.IsNull())
                    m_instance = new GlobalAssemblyCache();
                return m_instance;
            }
        }

        #endregion

        private GlobalAssemblyCache()
        {
            
        }

        /// <summary>
        /// Get normalized assembly name from COM-wrapper
        /// </summary>
        /// <param name="fusionName"></param>
        /// <returns></returns>
        private AssemblyName GetManagedName(IAssemblyName fusionName)
        {
            var ccCount = default(uint);
            var hr = fusionName.GetDisplayName(null, ref ccCount, ASM_NAME_DISPLAY_FLAGS.ALL);
            if (hr == /*E_INSUFFICIENT_BUFFER*/0x8007007a && ccCount > 0)
            {
                StringBuilder result = new StringBuilder((int)ccCount);
                fusionName.GetDisplayName(result, ref ccCount, ASM_NAME_DISPLAY_FLAGS.ALL);
                return new AssemblyName(result.ToString());
            }
            return null;
        }


        #region ICollection<AssemblyName> Members

        /// <summary>
        /// Get count of assemblies
        /// </summary>
        public int Count
        {
            get 
            {
                var result = default(int);
                //Create unmanaged assembly enumerator
                var assemblyEnum = FusionWrapper.CreateAssemblyEnum();
                //Set iterator to start position
                assemblyEnum.Reset();
                var fusionName = default(IAssemblyName);
                //Get next assembly info
                ExceptionManager.ThrowComException(assemblyEnum.GetNextAssembly(IntPtr.Zero, out fusionName, 0));
                while (fusionName != null)
                {
                    //Converts unmanaged information about assembly into managed
                    result++;
                    Marshal.ReleaseComObject(fusionName);
                    //Get next assembly info
                    ExceptionManager.ThrowComException(assemblyEnum.GetNextAssembly(IntPtr.Zero, out fusionName, 0));
                }
                Marshal.ReleaseComObject(assemblyEnum);
                return result;
            }
        }

        bool ICollection<AssemblyName>.IsReadOnly
        {
            get { return false; }
        }

        /// <summary>
        /// Install new assembly into GAC
        /// </summary>
        /// <param name="assembly"></param>
        public void Add(AssemblyName assembly)
        {
            if (!InstallAssembly(new Uri(assembly.CodeBase).LocalPath))
                ExceptionManager.Throw<InvalidOperationException>("Cannot install assembly", "assembly");
        }

        [Obsolete("Do not use this method, because you uninstall all assemblies in GAC!")]
        void ICollection<AssemblyName>.Clear()
        {
            foreach (AssemblyName name in this)
                UninstallAssembly(name.Name);
        }

        /// <summary>
        /// Returns value indicating that specified assembly already installed in the GAC
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        public bool Contains(AssemblyName name)
        {
            return this.Exists(t => AssemblyName.ReferenceMatchesDefinition(t, name));
        }

        void ICollection<AssemblyName>.CopyTo(AssemblyName[] array, int arrayIndex)
        {
            this.ToArray().CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Uninstall assembly from GAC
        /// </summary>
        /// <param name="name">Name of the assembly</param>
        /// <returns>True, if assembly successully uninstalled</returns>
        public bool Remove(AssemblyName name)
        {
            UninstallAssembly(name.Name);
            return true;
        }
        #endregion

        #region IEnumerable Members

        /// <summary>
        /// Get iterator for GAC assemblies
        /// </summary>
        /// <returns></returns>
        public IEnumerator<AssemblyName> GetEnumerator()
        {
            //Create unmanaged assembly enumerator
            var assemblyEnum = FusionWrapper.CreateAssemblyEnum();
            //Set iterator to start position
            assemblyEnum.Reset();
            var fusionName = default(IAssemblyName);
            //Get next assembly info
            ExceptionManager.ThrowComException(assemblyEnum.GetNextAssembly(IntPtr.Zero, out fusionName, 0));
            while (fusionName != null)
            {
                //Converts unmanaged information about assembly into managed
                yield return GetManagedName(fusionName);
                Marshal.ReleaseComObject(fusionName);
                //Get next assembly info
                ExceptionManager.ThrowComException(assemblyEnum.GetNextAssembly(IntPtr.Zero, out fusionName, 0));
            }
            Marshal.ReleaseComObject(assemblyEnum);
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion
        
        /// <summary>
        /// Install assembly into GAC
        /// </summary>
        /// <param name="manifestFilePath">File path to assembly</param>
        /// <returns>True, if assembly successfully installed</returns>
        public static bool InstallAssembly(string manifestFilePath)
        {
            if (!System.IO.File.Exists(manifestFilePath))
                return false;
            var cache = FusionWrapper.CreateAssemblyCache();
            ExceptionManager.ThrowComException(
                cache.InstallAssembly(0, manifestFilePath, IntPtr.Zero));
            Marshal.ReleaseComObject(cache);
            return true;
        }

        /// <summary>
        /// Unistall assembly from GAC
        /// </summary>
        /// <param name="displayName">Display name of the assembly</param>
        /// <returns></returns>
        public static uint UninstallAssembly(string displayName)
        {
            IAssemblyCache cache = FusionWrapper.CreateAssemblyCache();
            var disposition = default(uint);
            ExceptionManager.ThrowComException(
                cache.UninstallAssembly(0, displayName, IntPtr.Zero, ref disposition));
            
            Marshal.ReleaseComObject(cache);
            return disposition;
        }
    }
}
