// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>The enumeration type that describes the precision level of the executing function.</summary>
#pragma warning disable CA1028
enum Precisions : byte
#pragma warning restore CA1028
{
    /// <summary>Use this value for no computation if evaluation has been pre-computed.</summary>
    None,

    /// <summary>Use this value for a cheap computation that ignores several edge-cases for performance.</summary>
    Low,

    /// <summary>Use this value for an expensive computation that exhaustively checks all edge-cases.</summary>
    High,
}
