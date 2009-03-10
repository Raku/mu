using System.Linq;
using System.Text;
using System.Data.Linq;
using System.Extensions;

namespace System.Collections.Generic.Extensions
{
    using MemoryStream = System.IO.MemoryStream;

    /// <summary>
    /// Represents extension methods for the IEnumerable generic interface
    /// </summary>
    [TypeExtender(typeof(IEnumerable<>))]
    public static class IEnumerableExtensions
    {
        /// <summary>
        /// Iterates through all sequence and performs specified action on each
        /// element
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="enumerable">Target enumeration</param>
        /// <param name="action">Action</param>
        /// <exception cref="System.ArgumentNullException">One of the input agruments is null</exception>
        public static void ForEach<T>(this IEnumerable<T> enumerable, Action<T> action)
        {
            ExceptionManager.CheckOnNull(enumerable, "enumerable");
            ExceptionManager.CheckOnNull(action, "action");
            foreach (var elem in enumerable)
                action(elem);
        }

        /// <summary>
        /// Converts a one sequence into another sequence
        /// </summary>
        /// <typeparam name="TInput">The type of the sequence that is to be converted.</typeparam>
        /// <typeparam name="TOutput">The type the input object is to be converted to.</typeparam>
        /// <param name="enumerable">Target enumeration</param>
        /// <param name="convert">Conversion procedure</param>
        /// <returns>Converted sequence</returns>
        /// <exception cref="System.ArgumentNullException">One of the input agruments is null</exception>
        public static IEnumerable<TOutput> Convert<TInput, TOutput>(this IEnumerable<TInput> enumerable, Converter<TInput, TOutput> convert)
        {
            //Checks all input arguments on null
            ExceptionManager.CheckOnNull(enumerable, "enumerable");
            ExceptionManager.CheckOnNull(convert, "convert");
            //Wraps Converter2 delegate into Converter
            Converter2<TInput, TOutput> convertEx = delegate(TInput input, out TOutput output)
            {
                output = convert(input);
                return true;
            };
            return Convert(enumerable, convertEx);
        }

        /// <summary>
        /// Converts a one sequence into another sequence
        /// </summary>
        /// <typeparam name="TInput">The type of the sequence that is to be converted.</typeparam>
        /// <typeparam name="TOutput">The type the input object is to be converted to.</typeparam>
        /// <param name="enumerable">Target enumeration</param>
        /// <param name="convert">Conversion procedure</param>
        /// <returns>Converted sequence</returns>
        /// <exception cref="System.ArgumentNullException">One of the input agruments is null</exception>
        public static IEnumerable<TOutput> Convert<TInput, TOutput>(this IEnumerable<TInput> enumerable, Converter2<TInput, TOutput> convert)
        {
            ExceptionManager.CheckOnNull(enumerable, "enumerable");
            ExceptionManager.CheckOnNull(convert, "convert");
            foreach (var item in enumerable)
            {
                var output = default(TOutput);
                if (convert(item, out output)) yield return output;
            }
        }

        /// <summary>
        /// Determines whether the enumerable contains elements that match the conditions defined by the specified predicate
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="enumerable">Target enumeration</param>
        /// <param name="predicate">Condition of the element to search for</param>
        /// <returns>true, if specified element is existed, otherwise, false</returns>
        /// <exception cref="System.ArgumentNullException">One of the input agruments is null</exception>
        public static bool Exists<T>(this IEnumerable<T> enumerable, Predicate<T> predicate)
        {
            ExceptionManager.CheckOnNull(enumerable, "enumerable");
            ExceptionManager.CheckOnNull(predicate, "predicate");
            foreach (var item in enumerable)
                if (predicate(item)) return true;
            return false;
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified predicate, and returns the first occurrence
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="enumerable">Target enumeration</param>
        /// <param name="predicate">Condition of the element to search for</param>
        /// <returns>Search result</returns>
        public static T Find<T>(this IEnumerable<T> enumerable, Predicate<T> predicate)
        {
            ExceptionManager.CheckOnNull(enumerable, "enumerable");
            ExceptionManager.CheckOnNull(predicate, "predicate");
            foreach (var item in enumerable)
                if (predicate(item)) return item;
            return default(T);
        }

        /// <summary>
        /// Converts the current sequence into DataTable
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="queryResult">Result of the LINQ-query that contains a sequence of elements</param>
        /// <param name="columns">Columns names, which must be included into table</param>
        /// <returns>Converted data table</returns>
        public static LinqDataTable<T> ToDataTable<T>(this IEnumerable<T> queryResult, params string[] columns)
        {
            return new LinqDataTable<T>(queryResult, columns);
        }

        /// <summary>
        /// Converts the current sequence into DataTable
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="queryResult">Result of the LINQ-query that contains a sequence of elements</param>
        /// <returns>Converted data table</returns>
        public static LinqDataTable<T> ToDataTable<T>(this IEnumerable<T> queryResult)
        {
            return new LinqDataTable<T>(queryResult);
        }

        public static IDictionary<TKey, TValue> ToDictionary<TKey, TValue, TElement>(this IEnumerable<TElement> enumerable, Converter<TElement, KeyValuePair<TKey, TValue>> converter)
        {
            //enumerable.
            if (enumerable.IsNull()) return null;
            ExceptionManager.CheckOnNull(converter, "converter");
            var result = new Dictionary<TKey, TValue>();
            enumerable.ForEach(t =>
              {
                  var pair = converter(t);
                  result.Add(pair.Key, pair.Value);
              });
            return result;
        }

        /// <summary>
        /// Adds one or more sequences to the end of the current sequence
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="target">Initial sequence</param>
        /// <param name="enums">Sequences to concat</param>
        /// <returns>United sequences</returns>
        public static IEnumerable<T> AddToEnd<T>(this IEnumerable<T> target, params IEnumerable<T>[] enums)
        {
            ExceptionManager.CheckOnNull(enums, "enums");
            if (target.IsNotNull())
                foreach (var item in target)
                    yield return item;
            foreach (var sequence in enums)
                if (sequence.IsNotNull())
                    foreach (var item in sequence)
                        yield return item;
        }

        /// <summary>
        /// Adds one or more sequences to the begin of the current sequence
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="target">Initial sequence</param>
        /// <param name="enums">Sequences to concat</param>
        /// <returns>United sequences</returns>
        public static IEnumerable<T> AddToBegin<T>(this IEnumerable<T> target, params IEnumerable<T>[] enums)
        {
            ExceptionManager.CheckOnNull(enums, "enums");
            foreach (var sequence in enums)
                if (sequence.IsNotNull())
                    foreach (var item in sequence)
                        yield return item;
            if (target.IsNotNull())
                foreach (var item in target)
                    yield return item;
        }

        /// <summary>
        /// Adds one or more elements to sequence
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="target">Initial sequence</param>
        /// <param name="enums">Elements to concat</param>
        /// <returns>United sequences</returns>
        public static IEnumerable<T> AddToEnd<T>(this IEnumerable<T> target, params T[] values)
        {
            return target.AddToEnd(values.UnsafeCast<IEnumerable<T>>());
        }

        /// <summary>
        /// Adds one or more elements to the begin of sequence
        /// </summary>
        /// <typeparam name="T">Sequence element type</typeparam>
        /// <param name="target">Initial sequence</param>
        /// <param name="enums">Elements to concat</param>
        /// <returns>United sequences</returns>
        public static IEnumerable<T> AddToBegin<T>(this IEnumerable<T> target, params T[] values)
        {
            return target.AddToBegin(values.UnsafeCast<IEnumerable<T>>());
        }

        public static IEnumerable<T> Delete<T>(this IEnumerable<T> target, int position, int length)
        {
            var pos = 0;
            if(target.IsNotNull())
                foreach (var item in target)
                {
                    if (pos == position && length > 0)
                    {
                        length--;
                        continue;
                    }
                    pos++;
                    yield return item;
                }
        }

        public static IEnumerable<T> Delete<T>(this IEnumerable<T> target, T element)
        {
            var pos = 0;
            if (target.IsNotNull())
                foreach (var item in target)
                {
                    if (item.IsNull())
                        if (element.IsNull()) continue;
                    if (item.Equals(element)) continue;
                    yield return item;
                }
        }

        public static string AsString<T>(this IEnumerable<T> target)
            where T : IConvertible
        {
            if (target.IsNull()) return null;
            var result = new StringBuilder();
            foreach (var str in target)
                result.Append(str.ToString(null));
            return result.ToString();
        }

        /// <summary>
        /// Converts an array to the string
        /// </summary>
        /// <typeparam name="T">Element type</typeparam>
        /// <param name="array">Target array</param>
        /// <param name="separator">Element type separator</param>
        /// <param name="format">The System.String specifying the format to use.-or- null to use the default
        ///     format defined for the type of the System.IFormattable implementation.</param>
        /// <param name="provider">The System.IFormatProvider to use to format the value.-or- null to obtain
        ///     the numeric format information from the current locale setting of the operating system.</param>
        /// <returns>Array string representation</returns>
        public static string AsString<T>(this IEnumerable<T> target, string separator, string format, IFormatProvider provider)
        {
            return string.Join(separator, target.FormatSequence(format, provider));
        }

        /// <summary>
        /// Converts an array to the string
        /// </summary>
        /// <typeparam name="T">Element type</typeparam>
        /// <param name="array">Target array</param>
        /// <param name="separator">Element type separator</param>
        /// <param name="format">The System.String specifying the format to use.-or- null to use the default
        ///     format defined for the type of the System.IFormattable implementation.</param>
        /// <returns>Array string representation</returns>
        public static string AsString<T>(this IEnumerable<T> target, string separator, string format)
        {
            return target.AsString(separator, format, null);
        }

        /// <summary>
        /// Converts an array element into array of strings
        /// </summary>
        /// <typeparam name="T">Element type</typeparam>
        /// <param name="array">Target array</param>
        /// <param name="format">The System.String specifying the format to use.-or- null to use the default
        ///     format defined for the type of the System.IFormattable implementation.</param>
        /// <param name="provider">The System.IFormatProvider to use to format the value.-or- null to obtain
        ///     the numeric format information from the current locale setting of the operating system.</param>
        /// <returns>Converter array</returns>
        public static string[] FormatSequence<T>(this IEnumerable<T> targer, string format, IFormatProvider provider)
        {
            ExceptionManager.CheckOnNull(targer, "array");
            return InternalFormatArray<T>(targer, format, provider).ToArray();
        }

        /// <summary>
        /// Converts an array element into array of strings
        /// </summary>
        /// <typeparam name="T">Element type</typeparam>
        /// <param name="array">Target array</param>
        /// <param name="format">The System.String specifying the format to use.-or- null to use the default
        ///     format defined for the type of the System.IFormattable implementation.</param>
        /// <returns>Converter array</returns>
        public static string[] FormatSequence<T>(this IEnumerable<T> target, string format)
        {
            return target.FormatSequence(format, null);
        }

        private static IEnumerable<string> InternalFormatArray<T>(IEnumerable<T> target, string format, IFormatProvider provider)
        {
            foreach (var element in target)
                if (element is IFormattable)
                {
                    var formattable = element.SafeCast<IFormattable>();
                    yield return formattable.ToString(format, provider);
                }
                else yield return element.ToString();
        }

        /// <summary>
        /// Converts sequence of bytes into stream
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static System.IO.Stream AsStream(this IEnumerable<byte> target)
        {
            if (target.IsNull()) return null;
            return new MemoryStream(target.ToArray());
        }

        /// <summary>
        /// Converts the current enumeration into bi-directional sequence
        /// </summary>
        /// <typeparam name="T">Type of enumeration elements</typeparam>
        /// <param name="target">Target enumeration</param>
        /// <returns>Bi-directional sequence</returns>
        public static Processing.SequenceNavigator<T> AsNavigator<T>(this IEnumerable<T> target)
        {
            return new System.Collections.Processing.SequenceNavigator<T>(target);
        }
    }
}
