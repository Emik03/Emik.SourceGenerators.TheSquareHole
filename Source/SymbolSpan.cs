// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Represents an exact amount of symbols.</summary>
/// <remarks><para>
/// Deepest layer of nesting found in common libraries cap at 7, such as:
/// <c>Microsoft.VisualStudio.TestPlatform.Utilities.Helpers.Interfaces.IFileHelper</c>.
/// </para><para>
/// God forbid if a type has more than 7 layers of nesting, it's not my problem.
/// </para></remarks>
[StructLayout(LayoutKind.Sequential)]
struct SymbolSpan
{
    /// <summary>Gets the length of <see cref="Span{T}"/> instances that comes from <see cref="SymbolSpan"/>.</summary>
    public const int Length = 7;

    [UsedImplicitly]
#pragma warning disable IDE0044
    ISymbol _a, _b, _c, _d, _e, _f, _g;
#pragma warning restore IDE0044

    static SymbolSpan()
    {
        if (Unsafe.SizeOf<SymbolSpan>() / Unsafe.SizeOf<ISymbol>() is not Length)
            throw new InvalidOperationException(SevereFailureMessage);
    }

    static string SevereFailureMessage =>
        $"sizeof({nameof(SymbolSpan)}) = {Unsafe.SizeOf<SymbolSpan>()}, sizeof({nameof(ISymbol)} = {Unsafe.SizeOf<ISymbol>()}. For this type to work, we need sufficient capacity! {Length}";

    /// <summary>Creates the uninitialized span of symbols.</summary>
    /// <param name="_">The discard, which is used to let the compiler track lifetimes.</param>
    /// <returns>The allocated span of size <see cref="Length"/>.</returns>
    [Inline, MethodImpl(MethodImplOptions.AggressiveInlining), Pure]
    public static Span<ISymbol> New(in bool _ = default)
    {
        Unsafe.SkipInit(out SymbolSpan span);
        return MemoryMarshal.CreateSpan(ref span._a, Length);
    }
}
