namespace System.Extensions
{
    /// <summary>
    /// Represents extension methods for the System.String class
    /// </summary>
    [TypeExtender(typeof(string))]
    public static class StringExtensions
    {
        /// <summary>
        /// Indciates that the current string is null or have no characters
        /// </summary>
        /// <param name="value">Target string</param>
        /// <returns>True, the current string is null or have no characters; otherwise, false</returns>
        public static bool IsNullOrEmpty(this string value)
        {
            return value.IsNull() || value.Length.IsZero();
        }

        /// <summary>
        /// Delete a specified string from the current string
        /// </summary>
        /// <param name="value">Value to modify</param>
        /// <param name="strToDelete">Substring, which is deleted from value</param>
        /// <returns>Modified string</returns>
        public static string Delete(this string value, string strToDelete)
        {
            if (value.IsNullOrEmpty()) return null;
            return value.Replace(strToDelete, "");
        }

        /// <summary>
        /// Converts the current string value into enum field
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <param name="ignoreCase"></param>
        /// <returns></returns>
        public static T AsEnum<T>(this string value, bool ignoreCase)
            where T : struct, IComparable, IConvertible, IFormattable
        {
            ExceptionManager.CheckOnNull(value, "value");
            var enumType = typeof(T);
            //If specified type is not enum then throws exception
            if (!enumType.IsEnum)
                ExceptionManager.Throw<InvalidOperationException>();
            return Enum.Parse(enumType, value, ignoreCase).UnsafeCast<T>();
        }

        /// <summary>
        /// Converts the current string value into enum field
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        public static T AsEnum<T>(this string value)
            where T : struct, IComparable, IConvertible, IFormattable
        {
            return value.AsEnum<T>(false);
        }
    }
}
