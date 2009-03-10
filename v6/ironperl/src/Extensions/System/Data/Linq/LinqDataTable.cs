using System.Collections.Generic;
using System.Data.Linq.Mapping;
using System.Reflection;
using System.Reflection.Extensions;
using System.Runtime.Serialization;

namespace System.Data.Linq
{
    using StringCollection = System.Collections.Specialized.StringCollection;

    /// <summary>
    /// Типизированная обёртка для стадартной таблицы данных.
    /// Разработана для конвертирования LINQ-результатов в таблицы данных
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [Serializable]
    public class LinqDataTable<T>: DataTable, IEnumerable<T>
    {
        public LinqDataTable()
        {
            
        }

        public LinqDataTable(string tableName)
            : base(tableName)
        {

        }

        protected LinqDataTable(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }

        public LinqDataTable(IEnumerable<T> queryResult)
        {
            LoadScheme(typeof(T));
            foreach (var item in queryResult)
                LoadDataRow(item, true);
        }

        public LinqDataTable(IEnumerable<T> queryResult, params string[] columns)
        {
            //Загружаем схему таблицы по типу данных
            LoadScheme(typeof(T));
            var columnNames = new StringCollection();
            //Загружаем список имён колонок, которые должны присутствовать в таблице
            columnNames.AddRange(columns);
            //Эта коллекция содержит все колонки, имена которых не входят в список columns
            var colsToDelete = new List<DataColumn>();
            foreach (DataColumn col in Columns)
            {
                if (columnNames.Contains(col.ColumnName)) continue;
                colsToDelete.Add(col);
            }
            //Теперь удаляем ненужные колонки из схемы
            foreach (var col in colsToDelete)
                Columns.Remove(col);
            foreach (var item in queryResult)
                LoadDataRow(item, true, columns);
        }

        private static bool IsValidColumnType(Type targetType)
        {
            if (targetType == null) return false;
            if (targetType.Equals(typeof(TimeSpan))) return true;
            if (targetType.Equals(typeof(byte[]))) return true;
            return Type.GetTypeCode(targetType) != TypeCode.Object;
        }

        public static LinqDataTable<T> Create<G>(IEnumerable<G> source, Converter<G, T> converter)
        {
            return new LinqDataTable<T>(Convert(source, converter));
        }

        public void LoadDataRow(T newRow, bool fAcceptChanges)
        {
            var columns = new List<string>();
            foreach (DataColumn col in Columns)
                columns.Add(col.ColumnName);
            LoadDataRow(newRow, fAcceptChanges, columns.ToArray());
        }

        public void LoadDataRow(T newRow, bool fAcceptChanges, params string[] columns)
        {
            var values = new List<object>();
            var cols = new StringCollection();
            cols.AddRange(columns);
            foreach (var prop in newRow.GetType().GetProperties())
                if (cols.Contains(GetColumnName(prop)))
                    values.Add(prop.GetValue(newRow, null));
            LoadDataRow(values.ToArray(), fAcceptChanges);
        }

        private static string GetColumnName(PropertyInfo prop)
        {
            var result = prop.Name;
            if (Attribute.IsDefined(prop, typeof(ColumnAttribute)))
            {
                var attr = Attribute.GetCustomAttribute(prop, typeof(ColumnAttribute)) as ColumnAttribute;
                if (!String.IsNullOrEmpty(attr.Name)) result = attr.Name;
            }
            return result;
        }

        protected void LoadScheme(Type targetType)
        {
            foreach (var prop in targetType.GetProperties())
            {
                var newColumn = Columns.Add();
                newColumn.ColumnName = newColumn.Caption = prop.Name;
                if (prop.IsDefined<ColumnAttribute>())
                {
                    var attr = prop.GetCustomAttribute<ColumnAttribute>();
                    if (!String.IsNullOrEmpty(attr.Name))
                        newColumn.ColumnName = newColumn.Caption = attr.Name;
                    newColumn.AllowDBNull = attr.CanBeNull;
                    newColumn.AutoIncrement = attr.IsDbGenerated;
                    newColumn.Expression = attr.Expression;
                    newColumn.Unique = attr.IsPrimaryKey;
                }
                if (IsValidColumnType(prop.PropertyType))
                    newColumn.DataType = prop.PropertyType;
                else continue;
            }
        }

        private static IEnumerable<T> Convert<G>(IEnumerable<G> source, Converter<G, T> converter)
        {
            foreach (var item in source)
                yield return converter(item);
        }

        #region IEnumerable<T> Members

        public IEnumerator<T> GetEnumerator()
        {
            foreach (DataRow row in Rows)
                yield return GetTypedRow(row);
        }

        #endregion

        /// <summary>
        /// Not implemented...yet
        /// </summary>
        /// <param name="row"></param>
        /// <returns></returns>
        public T GetTypedRow(DataRow row)
        {
            throw new NotImplementedException();
        }

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion
    }
}
