// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Contains various predicates for <see cref="ISymbol"/> and its derivatives.</summary>
static class SymbolPredicates
{
    /// <summary>Determines whether both symbols have potential to be an implementation of each other.</summary>
    /// <param name="left">The symbol to check.</param>
    /// <param name="right">The symbol to compare it to.</param>
    /// <returns>The value <see langword="true"/> if both parameters have similar metadata.</returns>
    public static bool IsCandidate(this ISymbol? left, ISymbol? right) =>
        left.IsSameBaseKindAs(right) &&
        left.IsStatic == right.IsStatic &&
        (left is IArrayTypeSymbol or IParameterSymbol or IPointerTypeSymbol ||
            !left.Name.IsNullOrEmpty() && left.Name == right.Name) &&
        (left.ContainingNamespace is not { IsGlobalNamespace: false, Name: not null and not "" } leftContainer ||
            right.ContainingNamespace is not { IsGlobalNamespace: false, Name: not null and not "" } rightContainer ||
            leftContainer.IsCandidate(rightContainer));

    /// <summary>Determines whether the symbol is an implicit declaration for an interface.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/> is
    /// an explicit declaration for an interface, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool IsExplicitInterfaceDefinition([NotNullWhen(true)] this ISymbol? symbol) =>
        symbol is { Name: var name } && !name.AsSpan().FindInterface().Equals(default);

    /// <summary>Determines whether the symbol can be extended.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/> can be extended with interfaces.
    /// </returns>
    public static bool IsExtendable([NotNullWhen(true)] this INamedTypeSymbol? symbol) =>
        symbol is { IsStatic: false, IsRefLikeType: false, DeclaringSyntaxReferences.IsEmpty: false } &&
        symbol.IsCompletelyPartial();

    /// <summary>Determines whether the symbol is accessible from an external assembly.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/> is accessible externally.
    /// </returns>
    public static bool IsFullyAccessible(this ISymbol symbol) =>
        symbol.DeclaredAccessibility.IsAccessible() &&
        !symbol.IsExplicitInterfaceDefinition() &&
        symbol.ContainingType.FindPathToNull(x => x.ContainingType).All(IsFullyAccessible);

    /// <summary>Determines whether the symbol is accessible from an external assembly.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <param name="assembly">The symbol that has its internals exposed.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// is accessible externally, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool IsFullyAccessible(this ISymbol symbol, IAssemblySymbol assembly) =>
        AssemblySymbolComparer.Equal(symbol.ContainingAssembly, assembly) || symbol.IsFullyAccessible();

    /// <summary>Gets the singular argument type from the symbol.</summary>
    /// <param name="symbol">The symbol to get the argument type from.</param>
    /// <returns>The <see cref="INamedTypeSymbol"/> that is the type of the argument.</returns>
    public static bool IsGenericEquals([NotNullWhen(true)] this ISymbol? symbol) =>
        symbol is IMethodSymbol { IsStatic: false, Name: nameof(Equals), Parameters: [{ Type: ITypeParameterSymbol }] };

    /// <summary>Determines whether the symbol is ignored.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// has the attribute to be ignored, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool IsIgnored(this ISymbol symbol) => symbol.HasAttribute("NoStructuralTyping");

    /// <summary>Determines whether the symbol has a legal name.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>The value <see langword="true"/> if the parameter <paramref name="symbol"/> has a legal name.</returns>
    public static bool IsLegalName(this ISymbol symbol)
    {
        var span = symbol.Name.AsSpan();

        if (span.IsEmpty)
            return false;

        foreach (var x in symbol.Name.AsSpan())
            if (x is '<' or '>' or ' ' or '$')
                return false;

        return true;
    }

    /// <summary>Determines whether the symbol is a member of some <see cref="ITypeSymbol"/>.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// is one of dozen types that can be defined in an interface.
    /// </returns>
    public static bool IsMember([NotNullWhen(true)] this ISymbol? symbol) =>
        symbol is (IFieldSymbol or IEventSymbol or IMethodSymbol or IPropertySymbol) and
            not IMethodSymbol { Name: ".ctor" or "..ctor" };

    /// <summary>Determines whether the symbol is null or the global namespace.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// is <see langword="null"/> or <see cref="INamespaceSymbol.IsGlobalNamespace"/>.
    /// </returns>
    public static bool IsNullOrGlobalNamespace([NotNullWhen(false)] this ISymbol? symbol) =>
        symbol is null or INamespaceSymbol { IsGlobalNamespace: true };

    /// <summary>Determines whether the implementation is redundant.</summary>
    /// <param name="type">The symbol to check.</param>
    /// <param name="inter">The symbol to compare to.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="type"/> implements <paramref name="inter"/>
    /// in an implicit manner by being a record type, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool IsRedundant(this ITypeSymbol type, ISymbol inter) =>
        (type, inter) is ({ IsRecord: true },
        {
            ContainingSymbol: null or INamespaceSymbol { Name: nameof(System), ContainingSymbol: null },
            Name: nameof(ICloneable),
        });

    /// <summary>Determines whether both symbols are of the same member type as each other.</summary>
    /// <param name="left">The symbol to check.</param>
    /// <param name="right">The symbol to compare it to.</param>
    /// <returns>
    /// The value <see langword="true"/> if both parameters implement the same higher-order interface.
    /// </returns>
    public static bool IsSameBaseKindAs([NotNullWhen(true)] this ISymbol? left, [NotNullWhen(true)] ISymbol? right) =>
        left switch
        {
            ITypeSymbol => right is ITypeSymbol,
            IAliasSymbol => right is IAliasSymbol,
            IEventSymbol => right is IEventSymbol,
            IFieldSymbol => right is IFieldSymbol,
            ILabelSymbol => right is ILabelSymbol,
            ILocalSymbol => right is ILocalSymbol,
            IMethodSymbol => right is IMethodSymbol,
            IDiscardSymbol => right is IDiscardSymbol,
            IAssemblySymbol => right is IAssemblySymbol,
            IPropertySymbol => right is IPropertySymbol,
            IParameterSymbol => right is IParameterSymbol,
            INamespaceSymbol => right is INamespaceSymbol,
            IPreprocessingSymbol => right is IPreprocessingSymbol,
            IRangeVariableSymbol => right is IRangeVariableSymbol,
            _ => false,
        };

    /// <summary>Determines whether both symbols have potential to be an implementation of each other.</summary>
    /// <param name="left">The symbol to check.</param>
    /// <param name="right">The symbol to compare it to.</param>
    /// <returns>The value <see langword="true"/> if both parameters have similar metadata.</returns>
    public static bool IsSameOverload(this ISymbol left, ISymbol right)
    {
        while (!left.IsNullOrGlobalNamespace() && !right.IsNullOrGlobalNamespace())
        {
            if (left.Name != right.Name || !HasSameNumberOfTypeParameters(left, right))
                return false;

            left = left.ContainingSymbol;
            right = right.ContainingSymbol;
        }

        return true;
    }

    /// <summary>Determines whether the parameter has a constrain on itself.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// has a constrain on itself, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool IsSelfConstrained(this ITypeParameterSymbol symbol) =>
        symbol.ConstraintTypes.Contains(symbol.ContainingType);

    /// <summary>Tries to extract the type arguments.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <param name="args">The extracted type arguments.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// has type arguments, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool TryGetTypeArguments(
        [NotNullWhen(true)] this ISymbol? symbol,
        out ImmutableArray<ITypeSymbol> args
    ) =>
        symbol switch
        {
            INamedTypeSymbol { TypeArguments: var x and not [] } => (args = x) is var _,
            IMethodSymbol { TypeArguments: var x and not [] } => (args = x) is var _,
            _ => false,
        };

    /// <summary>Tries to extract the type parameters.</summary>
    /// <param name="symbol">The symbol to check.</param>
    /// <param name="args">The extracted type parameters.</param>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <paramref name="symbol"/>
    /// has type arguments, otherwise; <see langword="false"/>.
    /// </returns>
    public static bool TryGetTypeParameters(
        [NotNullWhen(true)] this ISymbol? symbol,
        out ImmutableArray<ITypeParameterSymbol> args
    ) =>
        symbol switch
        {
            INamedTypeSymbol { TypeParameters: var x and not [] } => (args = x) is var _,
            IMethodSymbol { TypeParameters: var x and not [] } => (args = x) is var _,
            _ => false,
        };

    static bool HasSameNumberOfTypeParameters(ISymbol left, ISymbol right) =>
        left.TryGetTypeParameters(out var leftTypes) == right.TryGetTypeParameters(out var rightTypes) &&
        (leftTypes.IsDefault || rightTypes.IsDefault
            ? leftTypes.IsDefault && rightTypes.IsDefault
            : leftTypes.Length == rightTypes.Length);
}
