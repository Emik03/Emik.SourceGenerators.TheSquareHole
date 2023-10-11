// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

using static Precisions;

sealed record Substitutes(
    INamedTypeSymbol Type,
    InterfaceTree Tree,
    Config Config,
    Precisions Precision,
    SmallList<TypeEntry> Mapper = default
)
{
    const string
        TestInterface = "ISquare",
        TestType = "Cylinder";

    /// <summary>Gets the interface.</summary>
    public INamedTypeSymbol Interface => Tree.Node;

    /// <summary>Determines whether the names of the types match.</summary>
    /// <param name="typeSymbol">The type to test.</param>
    /// <param name="interSymbol">The interface to test.</param>
    /// <param name="type">The type pattern.</param>
    /// <param name="inter">The interface pattern.</param>
    /// <returns>The value determining whether the parameters match the names of the properties.</returns>
    public static bool IsMatch(
        ISymbol? typeSymbol,
        ISymbol? interSymbol,
        string type = TestType,
        string inter = TestInterface
    ) =>
        typeSymbol.FindPathToNull(x => x.ContainingSymbol).Any(x => x.Name.Contains(type)) &&
        interSymbol.FindPathToNull(x => x.ContainingSymbol).Any(x => x.Name.Contains(inter));

    /// <summary>Determines whether the names of the types match.</summary>
    /// <param name="symbol">The type or interface to test.</param>
    /// <param name="type">The type pattern.</param>
    /// <param name="inter">The interface pattern.</param>
    /// <returns>The value determining whether the parameters match the names of the properties.</returns>
    public static bool IsMatch(ISymbol? symbol, string type = TestType, string inter = TestInterface) =>
        symbol.FindPathToNull(x => x.ContainingSymbol).Any(x => x.Name.Contains(type) || x.Name.Contains(inter));

    /// <summary>Determines whether the names of the types match.</summary>
    /// <param name="tuple">The types to test.</param>
    /// <param name="type">The type pattern.</param>
    /// <param name="inter">The interface pattern.</param>
    /// <returns>The value determining whether the parameters match the names of the properties.</returns>
    public static bool IsMatch(
        (ISymbol? Type, ISymbol? Interface) tuple,
        string type = TestType,
        string inter = TestInterface
    ) =>
        IsMatch(tuple.Type, tuple.Interface, type, inter);

    /// <summary>Creates the collection of possible implementations of the type.</summary>
    /// <param name="type">The type that potentially implements interfaces.</param>
    /// <param name="tree">All interfaces in the hierarchical structure.</param>
    /// <param name="provider">The provider of options for the analyzer.</param>
    /// <returns>The collection of possible implementations.</returns>
    public static IEnumerable<Substitutes> From(
        INamedTypeSymbol type,
        InterfaceTree tree,
        AnalyzerConfigOptionsProvider provider
    ) =>
        tree.Children is var children &&
        Config.From(provider, type) is var config &&
        new Substitutes(type, tree, config, Low).CanImplement()
            ? From(type, tree, config)
               .SelectMany((x, i) => i is 0 ? children.SelectMany(x => From(type, x, provider)).Prepend(x) : x.Yield())
            : Enumerable.Empty<Substitutes>();

    /// <summary>Creates the collection of possible implementations of the type.</summary>
    /// <param name="type">The type that implements the interface.</param>
    /// <param name="tree">The interface to implement.</param>
    /// <param name="config">The configuration for the source generator.</param>
    /// <returns>The collection of possible implementations.</returns>
    // ReSharper restore NullableWarningSuppressionIsUsed
    public static IEnumerable<Substitutes> From(INamedTypeSymbol type, InterfaceTree tree, Config config)
    {
        IEnumerable<Substitutes> Singular(Precisions precision) =>
            new Substitutes(type, tree, config, precision) is var potential && potential.CanImplement()
                ? potential.Yield()
                : Enumerable.Empty<Substitutes>();

        var inter = tree.Node;

        if (type.IsRedundant(inter))
            return Enumerable.Empty<Substitutes>();

        if (type.FindImplementationForInterfaceMember(inter) is not null)
            return Singular(None);

        if (inter.GetMembers().IsEmpty || inter.IsObsolete())
            return Enumerable.Empty<Substitutes>();

        var generics = inter
           .FindPathToNull(x => x.ContainingType)
           .SelectMany(x => x.TypeParameters)
           .ToListLazily();

        if (generics is [])
            return Singular(High);

        if (generics.Where(x => !x.IsSelfConstrained()).Skip(config.MaxSubstitutionDepth).Any())
            return Enumerable.Empty<Substitutes>();

        var potentialGenerics = type
           .FindPathToNull(x => x.BaseTypeThatHasParents())
           .SelectMany(x => x.GetMembers())
           .SelectMany(Extract.AllTypes)
           .Where(IncludedSyntaxNodeRegistrant.CanBeGeneric)
           .ToSetLazily(TypeSymbolComparer.Default);

        return generics
           .Select(Compliant)
           .ToSmallList()
           .Combinations()
           .Select(x => new Substitutes(type, tree, config, High, x))
           .Where(x => x.CanImplement());

        SmallList<TypeEntry> Compliant(ITypeParameterSymbol param) =>
            param.IsSelfConstrained() && new TypeEntry(param, type) is var entry
                ? entry.PartialComplies() ? entry : default
                : potentialGenerics.Select(x => new TypeEntry(param, x)).Where(y => y.Complies()).ToSmallList();
    }

    /// <inheritdoc />
    public bool Equals(Substitutes? x) =>
        x is var (type, (inter, _), _, _, mapper) &&
        TypeSymbolComparer.Equal(Type, type) &&
        TypeSymbolComparer.Equal(Interface, inter) &&
        Mapper.SequenceEqual(mapper);

    /// <summary>Determines whether this instance can co-exist with another.</summary>
    /// <param name="other">The <see cref="Substitutes"/> instance to compare.</param>
    /// <returns>
    /// The value <paramref langword="true"/> if both instances can co-exist, otherwise; <paramref langword="false"/>.
    /// </returns>
    public bool CanCoexistWith(Substitutes other)
    {
        static bool SameGenerics<T>(ICollection<T> first, ICollection<T> second, Func<T, ISymbol> func)
        {
            bool SameGeneric((T First, T Second) x) =>
                x is var (y, z) &&
                (func(y), func(z)) is var (l, r) &&
                l.Name == r.Name &&
                l.TryGetTypeArguments(out var left) == r.TryGetTypeArguments(out var right) &&
                (!left.IsDefault || SameGenerics(left, right, x => x));

            return first.Count == second.Count || first.Zip(second).All(SameGeneric);
        }

        return !NamedTypeSymbolComparer.Equal(Interface, other.Interface) ||
            !IsMapperGeneric() && !other.IsMapperGeneric() ||
            !SameGenerics(Mapper, other.Mapper, x => x.Value);
    }

    /// <summary>Determines whether the interface can be implemented in its current type substitution.</summary>
    /// <returns>
    /// The value <see langword="true"/> if <see cref="Type"/> can implement <see cref="Interface"/>
    /// based on the current state of <see cref="Mapper"/>, otherwise; <see langword="false"/>.
    /// </returns>
    public bool CanImplement()
    {
        bool Lossy(ISymbol x) => CanImplement(x, SymbolPredicates.IsCandidate);

        bool Precise(ISymbol x) => CanImplement(x, Implements);

        if (Precision is None || Type.GetMembers() is var members && members.Any(IsSame))
            return true;

        if (Interface.GetMembers() is var all && Type.IsRecord && all.Any(SymbolPredicates.IsGenericEquals))
            return Mapper is [var x] && x.IsObjectOr(Type);

        Func<ISymbol, bool> requirement = Precision switch
        {
            None => throw Unreachable,
            Low => Lossy,
            High => Precise,
            _ => throw Unreachable,
        };

        return all.All(requirement);
    }

    /// <summary>Determines whether the interface can be implemented in its current type substitution.</summary>
    /// <param name="inter">The <see cref="INamedTypeSymbol"/> to check for implementation.</param>
    /// <returns>
    /// The value <see langword="true"/> if <see cref="Type"/> can implement <paramref name="inter"/>
    /// based on the current state of <see cref="Mapper"/>, otherwise; <see langword="false"/>.
    /// </returns>
    public bool CanImplementParent(InterfaceTree inter) =>
        this with
        {
            Tree = inter,
            Precision = High,
            Mapper = TypeEntry.From(Mapper, inter.Node, Interface).ToSmallList(),
        } is var parent &&
        parent.CanImplement() &&
        parent.CanImplementParents();

    /// <summary>Determines whether the interface can be implemented in its current type substitution.</summary>
    /// <returns>
    /// The value <see langword="true"/> if <see cref="Type"/> can implement all interfaces from <see cref="Interface"/>
    /// based on the current state of <see cref="Mapper"/>, otherwise; <see langword="false"/>.
    /// </returns>
    public bool CanImplementParents() => Tree.Parents.All(CanImplementParent);

    /// <summary>Determines whether both symbols are considered the same.</summary>
    /// <param name="left">The left-hand side.</param>
    /// <param name="right">The right-hand side.</param>
    /// <returns>Whether both symbols are considered the same.</returns>
    // ReSharper disable once FunctionComplexityOverflow
    public bool Implements(ISymbol? left, ISymbol? right) =>
        Substitute(right) is var sub &&
        SameAnnotations(left, sub) &&
        Underlying(left, sub) is var (l, r) &&
        l.IsCandidate(r) &&
        l switch
        {
            IArrayTypeSymbol a when r is IArrayTypeSymbol b =>
                a.IsSZArray == b.IsSZArray &&
                a.Rank == b.Rank &&
                Implements(a.ElementType, b.ElementType) &&
                a.LowerBounds.GuardedSequenceEqual(b.LowerBounds) &&
                a.Sizes.GuardedSequenceEqual(b.Sizes),
            IDynamicTypeSymbol when r is IDynamicTypeSymbol => true,
            IEventSymbol a when r is IEventSymbol b => Implements(a.Type, b.Type),
            IFieldSymbol a when r is IFieldSymbol b => Implements(a.Type, b.Type),
            IFunctionPointerTypeSymbol a when r is IFunctionPointerTypeSymbol b => Implements(
                a.Signature,
                b.Signature
            ),
            IMethodSymbol a when r is IMethodSymbol b => Implements(a.ReturnType, b.ReturnType) &&
                a.Parameters.GuardedSequenceEqual(b.Parameters, Implements) &&
                a.TypeArguments.GuardedSequenceEqual(b.TypeArguments, Implements) &&
                a.TypeParameters.GuardedSequenceEqual(b.TypeParameters, Implements),
            INamedTypeSymbol a when r is INamedTypeSymbol b =>
                a.TypeArguments.GuardedSequenceEqual(b.TypeArguments, Implements),
            IParameterSymbol a when r is IParameterSymbol b => (!Config.IncludeParameterName || a.Name == b.Name) &&
                a.RefKind == b.RefKind &&
                Implements(a.Type, b.Type),
            IPointerTypeSymbol a when r is IPointerTypeSymbol b => Implements(a.PointedAtType, b.PointedAtType),
            IPropertySymbol a when r is IPropertySymbol b => Implements(a.Type, b.Type) &&
                a.Parameters.GuardedSequenceEqual(b.Parameters, Implements),
            ITypeParameterSymbol when r is ITypeParameterSymbol => true,
            _ => false,
        };

    /// <summary>Determines whether the mapper's values are all generic.</summary>
    /// <returns>
    /// The value <paramref langword="true"/> if all values in <see cref="Mapper"/> are
    /// <see cref="ITypeParameterSymbol"/>, otherwise; <paramref langword="false"/>.
    /// </returns>
    public bool IsMapperGeneric() => Mapper.All(y => y.Value is ITypeParameterSymbol);

    /// <summary>Determines whether the names of the types match.</summary>
    /// <returns>The value determining whether the parameters match the names of the properties.</returns>
    public bool IsMatch() => IsMatch(TestType, TestInterface);

    /// <summary>Determines whether the names of the types match.</summary>
    /// <param name="type">The type pattern.</param>
    /// <param name="inter">The interface pattern.</param>
    /// <returns>The value determining whether the parameters match the names of the properties.</returns>
    public bool IsMatch(string type, string inter = TestInterface) =>
        Type.Name.Contains(type) && Interface.Name.Contains(inter);

    /// <summary>Determines whether the names of the types match.</summary>
    /// <typeparam name="T">The type of <paramref name="_"/>.</typeparam>
    /// <param name="_">The discard.</param>
    /// <returns>The value determining whether the parameters match the names of the properties.</returns>
    public bool IsMatch<T>(T _) => /* t is not false && */ IsMatch(TestType, TestInterface);

    /// <inheritdoc />
    public override int GetHashCode()
    {
        const int Prime = 397;

        unchecked
        {
            var targetSum = TypeSymbolComparer.GetHashCode(Type) * Prime ^ TypeSymbolComparer.GetHashCode(Interface);

            var typeArgumentSum = Mapper
               .Aggregate(targetSum, (a, n) => a * Prime ^ TypeSymbolComparer.GetHashCode(n.Value));

            return targetSum * Prime ^ typeArgumentSum;
        }
    }

    /// <summary>Gets the precedence, where a higher number means it needs to be inserted first.</summary>
    /// <returns>The precedence value.</returns>
    public int Precedent() =>
        (IsMapperGeneric() ? Mapper.Count + 1 : 0) +
        Mapper.Count(x => x.Value.NullableAnnotation is NullableAnnotation.Annotated);

    /// <summary>Gets the name of the symbol.</summary>
    /// <param name="symbol">The symbol to get the name of.</param>
    /// <param name="returnGlobalAlias">Whether or not to include the global alias.</param>
    /// <returns>The name of the parameter <paramref name="symbol"/>.</returns>
    public string NameOf(INamespaceOrTypeSymbol symbol, bool returnGlobalAlias = true) =>
        NameOfContaining(symbol, NameOfCurrent(symbol)) is var name && returnGlobalAlias ? $"global::{name}" : name;

    /// <summary>Gets the name of the current interface.</summary>
    /// <returns>The name of the member <see cref="Interface"/>.</returns>
    public string NameOfInterface() => NameOf(Interface);

    bool CanImplement(ISymbol x, Func<ISymbol, ISymbol, bool> func)
    {
        if (x.HasDefaultImplementation())
            return true;

        // ReSharper disable once LoopCanBeConvertedToQuery
        foreach (var next in MembersWithPotential(Type))
            if (func(next, x))
                return true;

        return false;
    }

    static (ISymbol?, ISymbol?) Underlying(ISymbol? x, ISymbol? y) =>
        (x.UnderlyingNullable() ?? x, y.UnderlyingNullable() ?? y);

    static Span<ISymbol> MembersWithPotential(INamedTypeSymbol type)
    {
        static bool HasPotential(ISymbol x) =>
            x.IsAccessible() &&
            x.IsMember() &&
            x.IsLegalName() &&
            !x.IsExplicitInterfaceDefinition() &&
            !x.HasAttributeWithFullyQualifiedMetadataName(Of<AttributeGenerator>());

        return type
           .FindPathToNull(Extract.BaseTypeThatHasParents)
           .SelectMany(x => x.GetMembers())
           .Where(HasPotential)
           .ToArray();
    }

    static NullableAnnotation? AnnotationOf(ISymbol? x) => (x as IParameterSymbol)?.NullableAnnotation;

    bool IsSame(ISymbol symbol) =>
        symbol.Name.AsSpan().ExplicitInterface() is [.. var name, '.'] &&
        (name.IndexOf('<') is var open and not -1 ? open : name.Length) is var to &&
        name[..to].SplitAny(".".AsSpan()).Last().SequenceEqual(Interface.Name.AsSpan()) &&
        (Precision is Low || Mapper.HeadSpan(name[to..], static (x, y) => y.MatchesGenerics(x)));

    bool SameAnnotations(ISymbol? x, ISymbol? y) =>
        (!Config.IncludeNullability || AnnotationOf(x) == AnnotationOf(y)) &&
        x.UnderlyingNullable() is { IsValueType: true } == y.UnderlyingNullable() is { IsValueType: true };

    string FullyQualifiedNameOfSubstitute(ITypeSymbol x) =>
        Mapper
           .First(y => TypeSymbolComparer.Equal(y.Key, x))
           .Value
           .ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

    string NameOfContaining(ISymbol symbol, string name) =>
        (symbol.ContainingType as INamespaceOrTypeSymbol ??
            (symbol.ContainingNamespace is { IsGlobalNamespace: false } space ? space : null)) is { } parent
            ? $"{NameOf(parent, false)}.{name}"
            : name;

    string NameOfCurrent(ISymbol symbol) =>
        symbol is INamedTypeSymbol { TypeArguments: { IsEmpty: false } types }
            ? $"{symbol.Name}<{types.Select(FullyQualifiedNameOfSubstitute).Conjoin()}>"
            : symbol.Name;

    [return: NotNullIfNotNull(nameof(symbol))]
    ISymbol? Substitute(ISymbol? symbol)
    {
        if (symbol is not ITypeParameterSymbol x)
            return symbol;

        foreach (var (key, value) in Mapper)
            if (TypeSymbolComparer.Equal(x, key))
                return value;

        return symbol;
    }
}
