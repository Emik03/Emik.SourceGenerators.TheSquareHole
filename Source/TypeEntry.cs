// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Represents a type entry.</summary>
/// <param name="Key">The key.</param>
/// <param name="Value">The value.</param>
[CLSCompliant(false), StructLayout(LayoutKind.Sequential)]
public readonly record struct TypeEntry(ITypeParameterSymbol Key, ITypeSymbol Value)
{
    /// <summary>Constructs <see cref="TypeEntry"/> instances from the interface.</summary>
    /// <param name="mapper">The mapper to translate the generics from.</param>
    /// <param name="parent">The interface that contains values to create the instance.</param>
    /// <param name="child">The child that contains the context for the parameter <paramref name="parent"/>.</param>
    /// <returns>
    /// The <see cref="TypeEntry"/> instances with the values of the type arguments of <paramref name="parent"/>.
    /// </returns>
    public static IEnumerable<TypeEntry> From(
        ICollection<TypeEntry> mapper,
        INamedTypeSymbol parent,
        INamedTypeSymbol? child = null
    )
    {
        static IEnumerable<TypeEntry> Types(INamedTypeSymbol x) => x.TypeParameters.Zip(x.TypeArguments).Select(From);

        static ITypeSymbol? FindFrom(TypeEntry x, IEnumerable<TypeEntry> types) =>
            types
               .Where(y => !x.Key.Name.IsNullOrEmpty() && x.Key.Name == y.Key.Name)
               .ItemCanBeEmptyNullable()
               .FirstOrDefault()
              ?.Value;

        IEnumerable<TypeEntry> ChildContext() =>
            child?.AllInterfaces.FirstOrDefault(parent.IsSameOverload)?.Then(Types) ?? Enumerable.Empty<TypeEntry>();

        ITypeSymbol? Find(TypeEntry x) => FindFrom(x, mapper) ?? FindFrom(x, ChildContext());

        return Types(parent).Select(x => (x.Key, Find(x))).Select(TryFrom).Filter();
    }

    /// <summary>Constructs the instance of <see cref="TypeEntry"/> from the tuple.</summary>
    /// <param name="tuple">The tuple that contains values to create the instance.</param>
    /// <returns>
    /// The instance of <see cref="TypeEntry"/> with the values of the parameter <paramref name="tuple"/>.
    /// </returns>
    public static TypeEntry From((ITypeParameterSymbol Key, ITypeSymbol Value) tuple) => new(tuple.Key, tuple.Value);

    /// <summary>Constructs the instance of <see cref="TypeEntry"/> from the tuple.</summary>
    /// <param name="tuple">The tuple that contains values to create the instance.</param>
    /// <returns>
    /// The instance of <see cref="TypeEntry"/> with the values of the parameter <paramref name="tuple"/>.
    /// </returns>
    public static TypeEntry? TryFrom((ITypeParameterSymbol Key, ITypeSymbol? Value) tuple) =>
        tuple is (var key, { } value) ? new(key, value) : null;

    /// <summary>Determines whether the type complies with the generic constraints.</summary>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <see cref="Value"/> complies
    /// with <see cref="Key"/>'s constraints, otherwise; <see langword="false"/>.
    /// </returns>
    public bool Complies()
    {
        if (!PartialComplies())
            return false;

        var baseTypes = Value.FindPathToNull(x => x.BaseTypeThatHasParents());
        var inherits = Value.AllInterfaces.Concat(baseTypes).ToCollectionLazily();
        return Key.ConstraintTypes.All(x => inherits.Any(x.IsCandidate));
    }

    /// <inheritdoc />
    public bool Equals(TypeEntry other) =>
        TypeSymbolComparer.Equal(Key, other.Key) && TypeSymbolComparer.Equal(Value, other.Value);

    /// <summary>Determines whether the replacement is <see cref="object"/>, or the parameter.</summary>
    /// <param name="type">The parameter to compare against.</param>
    /// <returns>
    /// The value <see langword="true"/> if <see cref="Value"/> is <see cref="object"/>
    /// or the parameter <paramref name="type"/>, otherwise; <see langword="false"/>.
    /// </returns>
    public bool IsObjectOr(ITypeSymbol type) =>
        Value is { SpecialType: SpecialType.System_Object } || TypeSymbolComparer.Equal(type, Value);

    /// <summary>
    /// Determines whether the type complies with the generic constraints,
    /// without looking at the inner generics.
    /// </summary>
    /// <returns>
    /// The value <see langword="true"/> if the parameter <see cref="Value"/> mostly complies
    /// with <see cref="Key"/>'s constraints, otherwise; <see langword="false"/>.
    /// </returns>
    public bool PartialComplies() =>
        (!Key.HasValueTypeConstraint || Value.IsValueType) &&
        (!Key.HasReferenceTypeConstraint || Value.IsReferenceType) &&
        (!Key.HasUnmanagedTypeConstraint || Value.IsUnmanagedType) &&
        (!Key.HasNotNullConstraint || Value.NullableAnnotation is not NullableAnnotation.Annotated) &&
        (!Key.HasConstructorConstraint || Value.HasParameterlessConstructor());

    /// <inheritdoc />
    public override int GetHashCode() =>
        unchecked(TypeSymbolComparer.GetHashCode(Key) * 397 ^ TypeSymbolComparer.GetHashCode(Value));
}
