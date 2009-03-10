using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace System.Collections.Processing
{
    /// <summary>
    /// Represents a bidirectional sequence navigator
    /// </summary>
    public interface INavigationContext<T>
    {
        /// <summary>
        /// Get value, which position is defined by marker
        /// </summary>
        /// <param name="marker">Marker, which determines a value position</param>
        /// <returns></returns>
        T GetValueAtMarker(object marker);

        /// <summary>
        /// Get or set the current position of navigation
        /// </summary>
        object CurrentPosition { get; set; }

        /// <summary>
        /// Get marked, which determines the next value in the sequence.
        /// If marker is null then next value is not available
        /// </summary>
        object Next { get; }

        /// <summary>
        /// Get marker, which determines the previous value in the sequence.
        /// If marker is null the previous value is not available.
        /// </summary>
        object Previous { get; }

        /// <summary>
        /// Returns value indicating that specified marker is valid
        /// for the current navigator
        /// </summary>
        /// <param name="marker">Marker to be checked</param>
        /// <returns></returns>
        bool IsValidMarker(object marker);
    }
}
