// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Generates the attribute needed to use this analyzer.</summary>
[Generator]
public sealed class AttributeGenerator() : FixedGenerator(
    "Emik.NoStructuralTypingAttribute",
    """
    // <auto-generated/>
    // ReSharper disable RedundantNameQualifier
    // ReSharper disable once CheckNamespace
    namespace Emik
    {
        /// <summary>Signifies to ignore this when determining potential interfaces that can be implemented.</summary>
        /// <remarks>
        /// <para>
        /// The behavior of this attribute depends on what you apply it on:
        /// </para>
        /// <list type="table">
        ///    <listheader>
        ///        <term>Target</term>
        ///        <description>Behavior</description>
        ///    </listheader>
        ///    <item>
        ///        <term>Type Member</term>
        ///        <description>Ignores the type altogether. No generation is ever performed on it.</description>
        ///    </item>
        ///    <item>
        ///        <term>Member</term>
        ///        <description>Ignores only this particular member when evaluating potential interfaces.</description>
        ///    </item>
        /// </list>
        /// </remarks>
        [global::System.AttributeUsage(
            global::System.AttributeTargets.Class |
            global::System.AttributeTargets.Struct |
            global::System.AttributeTargets.Method |
            global::System.AttributeTargets.Property |
            global::System.AttributeTargets.Field |
            global::System.AttributeTargets.Event
        )]
        internal sealed class NoStructuralTypingAttribute : global::System.Attribute { }
    }
    """
);
