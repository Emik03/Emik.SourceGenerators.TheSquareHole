// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Defines the configurations for this source generator.</summary>
/// <param name="EnableConcurrency">Determines whether to enable concurrency for inspections. Obsolete.</param>
/// <param name="IncludeNullability">Determines whether or not to include nullability as a restriction.</param>
/// <param name="IncludeParameterName">Determines whether or not to include parameter names as a restriction.</param>
/// <param name="MaxSubstitutionDepth">
/// Determines the maximum number of type substitutions allowed for a given interface.
/// </param>
[StructLayout(LayoutKind.Auto)]
readonly record struct Config(
    [property: Obsolete($"{nameof(IIncrementalGenerator)} removes the need for concurrency")] bool EnableConcurrency,
    bool IncludeNullability,
    bool IncludeParameterName,
    byte MaxSubstitutionDepth
)
{
    const bool
        EnableConcurrencyFallback = false,
        IncludeNullabilityFallback = false,
        IncludeParameterNameFallback = false;

    const byte MaxSubstitutionDepthFallback = SmallList.InlinedLength;

    const string Prefix = "the_square_hole_";

    /// <summary>
    /// Creates the instance of <see cref="Config"/> based on the provided <see cref="AnalyzerConfigOptions"/>.
    /// </summary>
    /// <param name="options">The options to parse.</param>
    /// <returns>The options used by this source generator.</returns>
    public static Config From(AnalyzerConfigOptions options) =>
        new(
            ParseEnableConcurrency(options),
            ParseIncludeNullability(options),
            ParseParameterName(options),
            ParseMaxSubstitutionDepth(options)
        );

    /// <summary>
    /// Creates the instance of <see cref="Config"/> based on the provided <see cref="AnalyzerConfigOptionsProvider"/>.
    /// </summary>
    /// <param name="provider">The options to parse.</param>
    /// <param name="_">The discard cancellation token.</param>
    /// <returns>The options used by this source generator.</returns>
    public static Config From(AnalyzerConfigOptionsProvider provider, CancellationToken _) =>
        From(provider.GlobalOptions);

    /// <summary>
    /// Creates the instance of <see cref="Config"/> based on the provided <see cref="AnalyzerConfigOptionsProvider"/>.
    /// </summary>
    /// <param name="provider">The options to parse.</param>
    /// <param name="type">The type for context.</param>
    /// <returns>The options used by this source generator.</returns>
    public static Config From(AnalyzerConfigOptionsProvider provider, INamedTypeSymbol type) =>
        From(provider.GetOptions(type));

    static bool ParseEnableConcurrency(AnalyzerConfigOptions options) =>
        ParseBoolean(options, "enable_concurrency", EnableConcurrencyFallback);

    static bool ParseIncludeNullability(AnalyzerConfigOptions options) =>
        ParseBoolean(options, "include_nullability", IncludeNullabilityFallback);

    static bool ParseParameterName(AnalyzerConfigOptions options) =>
        ParseBoolean(options, "include_parameter_name", IncludeParameterNameFallback);

    static byte ParseMaxSubstitutionDepth(AnalyzerConfigOptions options) =>
        Math.Min(
            ParseByte(options, "max_substitution_depth", MaxSubstitutionDepthFallback),
            MaxSubstitutionDepthFallback
        );

    static bool ParseBoolean(AnalyzerConfigOptions options, string name, bool fallback) =>
        Key(name) is var key && options.TryGetValue(key, out var value) && bool.TryParse(value, out var ret)
            ? ret
            : fallback;

    static byte ParseByte(AnalyzerConfigOptions options, string name, byte fallback) =>
        Key(name) is var key && options.TryGetValue(key, out var value) && byte.TryParse(value, out var ret)
            ? ret
            : fallback;

    static string Key(string name) => $"{Prefix}{name}";
}
