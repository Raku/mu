using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices.ComTypes;
using System.Reflection;

namespace System.Runtime.InteropServices
{
    using HRESULT = System.UInt32;

    using ULONG32 = System.UInt32;

    using MD_TOKEN = System.UInt32;                 //Generic token
    using MD_MODULE = System.UInt32;                // Module token (roughly, a scope)
    using MD_TYPE_REF = System.UInt32;              // TypeRef reference (this or other scope)
    using MD_TYPE_DEF = System.UInt32;              // using in this scope
    using MD_FIELD_DEF = System.UInt32;             // Field in this scope
    using MD_METHOD_DEF = System.UInt32;            // Method in this scope
    using MD_PARAM_DEF = System.UInt32;             // param token
    using MD_INTERFACE_IMPL = System.UInt32;        // interface implementation token
    using MD_MEMBER_REF = System.UInt32;            // MemberRef (this or other scope)
    using MD_CUSTOM_ATTRIBUTE = System.UInt32;      // attribute token
    using MD_PERMISSION = System.UInt32;            // DeclSecurity
    using MD_SIGNATURE = System.UInt32;             // Signature object
    using MD_EVENT = System.UInt32;                 // event token
    using MD_PROPERTY = System.UInt32;              // property token
    using MD_MODULE_REF = System.UInt32;            // Module reference (for the imported modules)

    // Assembly tokens.
    using MD_ASSEMBLY = System.UInt32;              // Assembly token.
    using MD_ASSEMBLY_REF = System.UInt32;          // AssemblyRef token.
    using MD_FILE = System.UInt32;                  // File token.
    using MD_EXPORTED_TYPE = System.UInt32;         // ExportedType token.
    using MD_MANIFEST_RESOURCE = System.UInt32;     // ManifestResource token.
    using MD_TYPE_SPEC = System.UInt32;             // TypeSpec object
    using MD_GENERIC_PARAM = System.UInt32;         // formal parameter to generic type or method
    using MD_METHOD_SPEC = System.UInt32;           // instantiation of a generic method
    using MD_GENERIC_PARAM_CONSTRAINT = System.UInt32;// constraint on a formal generic parameter

    // Application string.
    using MD_STRING = System.UInt32;                // User literal string token.
    using MD_CPTOKEN = System.UInt32;               // constantpool token

    [ComImport]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    [Guid(RCWHelper.IID_IMetaDataImport)]
    internal interface IMetaDataImport
    {
        [PreserveSig]
        HRESULT CloseEnum(
            IntPtr hEnum
            );

        [PreserveSig]
        HRESULT CountEnum(
            IntPtr hEnum,
            [MarshalAs(UnmanagedType.I4)] out Int32 count
            );

        [PreserveSig]
        HRESULT EnumCustomAttributes(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.U4)] MD_TOKEN tk,
            [MarshalAs(UnmanagedType.U4)] MD_TOKEN tkType,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_CUSTOM_ATTRIBUTE[] rCustomAttributes,
            [MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcCustomAttributes
            );

        [PreserveSig]
        HRESULT EnumEvents(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_EVENT rEvents,
            [MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcEvents
            );

        [PreserveSig]
        HRESULT EnumFields(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF cl,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_FIELD_DEF rFields,
            [MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumFieldsWithName(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF cl,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_FIELD_DEF[] rFields,
            [MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumInterfaceImpls(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_INTERFACE_IMPL[] rImpls,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMemberRefs(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tkParent,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_MEMBER_REF[] rMemberRefs,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMembers(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF cl,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_TOKEN[] rMembers,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMembersWithName(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF cl,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_TOKEN[] rMembers,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMethodImpls(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_TOKEN[] rMethodBody,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_TOKEN[] rMethodDecl,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMethodSemantics(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_METHOD_DEF mb,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_TOKEN[] rEventProp,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMethods(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF cl,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_METHOD_DEF[] rMethods,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumMethodsWithName(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF cl,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_METHOD_DEF[] rMethods,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumModuleRefs(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_MODULE_REF[] rModuleRefs,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcModuleRefs
            );

        [PreserveSig]
        HRESULT EnumParams(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_METHOD_DEF cl,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_PARAM_DEF[] rParams,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumPermissionSets(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tk,
            [In, MarshalAs(UnmanagedType.U4)] UInt32 dwActions,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] MD_PERMISSION[] rPermission,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumProperties(
            [In, Out] ref IntPtr hEnum,
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] MD_PROPERTY[] rProperies,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcProperties
            );

        [PreserveSig]
        HRESULT EnumSignatures(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_SIGNATURE[] rSignatures,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcSignatures
            );

        [PreserveSig]
        HRESULT EnumTypeDefs(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_TYPE_DEF[] rTypeDefs,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTypeDefs
            );

        [PreserveSig]
        HRESULT EnumTypeRefs(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_TYPE_REF[] rTypeRefs,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTypeDefs
            );

        [PreserveSig]
        HRESULT EnumTypeSpecs(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_TYPE_SPEC[] rTypeSpecs,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTypeSpecs
            );

        [PreserveSig]
        HRESULT EnumUnresolvedMethods(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_TOKEN[] rMethods,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcTokens
            );

        [PreserveSig]
        HRESULT EnumUserStrings(
            [In, Out] ref IntPtr hEnum,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] MD_STRING[] rStrings,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcStrings
            );

        [PreserveSig]
        HRESULT FindField(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] byte[] pvSigBlob,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out MD_FIELD_DEF pmb
            );

        [PreserveSig]
        HRESULT FindMember(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] byte[] pvSigBlob,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN pmb
            );

        [PreserveSig]
        HRESULT FindMemberRef(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_REF td,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] byte[] pvSigBlob,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out MD_MEMBER_REF pmr
            );

        [PreserveSig]
        HRESULT FindMethod(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] byte[] pvSigBlob,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out MD_METHOD_DEF pmb
            );

        [PreserveSig]
        HRESULT FindTypeDefByName(
            [MarshalAs(UnmanagedType.LPWStr)] string szTypeDef,
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tkEnclosingClass,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF ptd
            );

        [PreserveSig]
        HRESULT FindTypeRef(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tkResolutionScope,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_REF ptr
            );

        [PreserveSig]
        HRESULT GetClassLayout(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwPackSize,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] long[] tFieldOffset,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcFieldOffset,
            [MarshalAs(UnmanagedType.I4)] out UInt32 pulClassSize
            );

        [PreserveSig]
        HRESULT GetCustomAttributeByName(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tkObj,
            [MarshalAs(UnmanagedType.LPWStr)] string szName,
            out IntPtr ppData,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbData
            );

        [PreserveSig]
        HRESULT GetCustomAttributeProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_CUSTOM_ATTRIBUTE cv,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN ptkObj,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN ptkType,
            out IntPtr ppBlob,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSize
            );

        [PreserveSig]
        HRESULT GetEventProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_EVENT ev,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF pClass,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szEvent,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchEvent,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchEvent,
            [MarshalAs(UnmanagedType.U4)] out EventAttributes pdwEventFlags,
            [MarshalAs(UnmanagedType.U4)] out UInt32 ptkEventType,
            [MarshalAs(UnmanagedType.U4)] out MD_METHOD_DEF pmdAddOn,
            [MarshalAs(UnmanagedType.U4)] out MD_METHOD_DEF pmdRemoveOn,
            [MarshalAs(UnmanagedType.U4)] out MD_METHOD_DEF pmdFire,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 11)] MD_METHOD_DEF[] rmdOtherMethod,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcOtherMethod
            );

        [PreserveSig]
        HRESULT GetFieldMarshal(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tk,
            out IntPtr ppvNativeType,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbNativeType
            );

        [PreserveSig]
        HRESULT GetFieldProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_FIELD_DEF md,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF pClass,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szField,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchField,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchField,
            [MarshalAs(UnmanagedType.U4)] out EventAttributes pdwAttr,
            out IntPtr ppvSigBlob,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwCPlusTypeFlag,
            out IntPtr ppValue,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcchValue
            );

        [PreserveSig]
        HRESULT GetInterfaceImplProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_INTERFACE_IMPL iiImpl,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF pClass,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN ptkIface
            );

        [PreserveSig]
        HRESULT GetMemberProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN mb,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF pClass,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] char[] szMember,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchMember,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchMember,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwAttr,
            out IntPtr ppvSigBlob,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pulCodeRVA,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwImplFlags,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwCPlusTypeFlag,
            out IntPtr ppValue,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcchValue
            );

        [PreserveSig]
        HRESULT GetMemberRefProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_MEMBER_REF mr,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN ptk,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szMember,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchMember,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchMember,
            out IntPtr ppvSigBlob,
            [MarshalAs(UnmanagedType.I4)] out Int32 pbSigBlob
            );

        [PreserveSig]
        HRESULT GetMethodProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_METHOD_DEF mb,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF pClass,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szMethod,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchMethod,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchMethod,
            [MarshalAs(UnmanagedType.U4)] out MethodAttributes pdwAttr,
            out IntPtr ppvSigBlob,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pulCodeRVA,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwImplFlags
            );

        [PreserveSig]
        HRESULT GetMethodSemantics(
            [In, MarshalAs(UnmanagedType.U4)] MD_METHOD_DEF mb,
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tkEventProp,
            [MarshalAs(UnmanagedType.U4)] out COR_METHOD_SEMANTICS_ATTR pdwSemanticsFlags
            );

        [PreserveSig]
        HRESULT GetModuleFromScope(
            [MarshalAs(UnmanagedType.U4)] out MD_MODULE pmd
            );

        [PreserveSig]
        HRESULT GetModuleRefProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_MODULE_REF mur,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] char[] szName,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchName,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchName
            );

        [PreserveSig]
        HRESULT GetNameFromToken(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tk,
            out IntPtr pszUtf8NamePtr
            );

        [PreserveSig]
        HRESULT GetNativeCallConvFromSig(
            IntPtr pvSig,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cbSig,
            [MarshalAs(UnmanagedType.U4)] out CallingConvention pCallConv
            );

        [PreserveSig]
        HRESULT GetNestedClassProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF tdNestedClass,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF ptdEnclosingClass
            );

        [PreserveSig]
        HRESULT GetParamForMethodIndex(
            [In, MarshalAs(UnmanagedType.U4)] MD_METHOD_DEF md,
            [In, MarshalAs(UnmanagedType.I4)] Int32 ulParamSeq,
            [MarshalAs(UnmanagedType.U4)] out MD_PARAM_DEF ppd
            );

        [PreserveSig]
        HRESULT GetParamProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_PARAM_DEF tk,
            [MarshalAs(UnmanagedType.U4)] out  MD_METHOD_DEF pmd,
            [MarshalAs(UnmanagedType.U4)] out  UInt32 pulSequence,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szName,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchName,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchName,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwAttr,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwCPlusTypeFlag,
            out IntPtr ppValue,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcchValue
            );

        [PreserveSig]
        HRESULT GetPermissionSetProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_PERMISSION pm,
            [MarshalAs(UnmanagedType.U4)] out System.Security.Permissions.SecurityAction pdwAction,
            out IntPtr ppvPermission,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbPermission
            );

        [PreserveSig]
        HRESULT GetPinvokeMap(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tk,
            [MarshalAs(UnmanagedType.U4)] out COR_PINVOKE_MAP pdwMappingFlags,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)] char[] szImportName,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchImportName,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchImportName,
            [MarshalAs(UnmanagedType.U4)] out MD_MODULE_REF pmrImportDLL
            );

        [PreserveSig]
        HRESULT GetPropertyProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_PROPERTY prop,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF pClass,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szProperty,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchProperty,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchProperty,
            [MarshalAs(UnmanagedType.U4)] out PropertyAttributes pdwPropFlags,
            out IntPtr ppvSigBlob,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSigBlob,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwCPlusTypeFlag,
            out IntPtr ppDefaultValue,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcchDefaultValue,
            [MarshalAs(UnmanagedType.U4)] out MD_METHOD_DEF pmdSetter,
            [MarshalAs(UnmanagedType.U4)] out MD_METHOD_DEF pmdGetter,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 14)] out MD_METHOD_DEF[] rmdOtherMethod,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cMax,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcOtherMethod
            );

        [PreserveSig]
        HRESULT GetRVA(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tk,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pulCodeRVA,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pdwImplFlags
            );

        [PreserveSig]
        HRESULT GetScopeProps(
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] char[] szName,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchName,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchName,
            [MarshalAs(UnmanagedType.Struct)] ref Guid mvid
            );

        [PreserveSig]
        HRESULT GetSigFromToken(
            [In, MarshalAs(UnmanagedType.U4)] MD_SIGNATURE mdSig,
            out IntPtr ppvSig,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSig
            );

        [PreserveSig]
        HRESULT GetTypeDefProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_DEF td,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szTypeDef,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchTypeDef,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchTypeDef,
            [MarshalAs(UnmanagedType.U4)] out TypeAttributes pdwTypeDefFlags,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN ptkExtends
            );

        [PreserveSig]
        HRESULT GetTypeRefProps(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_REF tr,
            [MarshalAs(UnmanagedType.U4)] out MD_TOKEN ptkResolutionScope,
            [MarshalAs(UnmanagedType.LPWStr)] StringBuilder szName,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchName,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchName
            );

        [PreserveSig]
        HRESULT GetTypeSpecFromToken(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_SPEC typespec,
            out IntPtr ppvSig,
            [MarshalAs(UnmanagedType.I4)] out Int32 pcbSig
            );

        [PreserveSig]
        HRESULT GetUserString(
            [In, MarshalAs(UnmanagedType.U4)] MD_STRING stk,
            [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] char[] szString,
            [In, MarshalAs(UnmanagedType.I4)] Int32 cchString,
            [MarshalAs(UnmanagedType.I4)] out Int32 pchString
            );

        [PreserveSig]
        HRESULT IsGlobal(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN pd,
            [MarshalAs(UnmanagedType.U4)] out UInt32 pbGlobal
            );

        [PreserveSig]
        HRESULT IsValidToken(
            [In, MarshalAs(UnmanagedType.U4)] MD_TOKEN tk
            );

        [PreserveSig]
        HRESULT ResetEnum(
            [In] IntPtr hEnum,
            [MarshalAs(UnmanagedType.U4)] UInt32 ulPos
            );

        [PreserveSig]
        HRESULT ResolveTypeRef(
            [In, MarshalAs(UnmanagedType.U4)] MD_TYPE_REF tr,
            [MarshalAs(UnmanagedType.Struct)] ref Guid riid,
            out IntPtr ppIScope,
            [MarshalAs(UnmanagedType.U4)] out MD_TYPE_DEF ptd
            );
    }
}
