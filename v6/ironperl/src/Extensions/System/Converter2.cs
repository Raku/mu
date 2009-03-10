using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System
{
    /// <summary>
    ///  Represents a method that converts an object from one type to another type.
    /// </summary>
    /// <typeparam name="TInput">The type of object that is to be converted.</typeparam>
    /// <typeparam name="TOutput">The type the input object is to be converted to.</typeparam>
    /// <param name="input">The object to convert.</param>
    /// <param name="output">The TOutput that represents the converted TInput.</param>
    /// <returns>true, if conversion is available, otherwise, false</returns>
    [Serializable]
    public delegate bool Converter2<TInput, TOutput>(TInput input, out TOutput output);
}
