// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

using static AttributeTargets;

/// <summary>Exists as a polyfill, since Emik.Morsels relies on its existence.</summary>
[AttributeUsage(Class | Struct | Method | Property | Field | Event)]
sealed class NoStructuralTypingAttribute : Attribute;
