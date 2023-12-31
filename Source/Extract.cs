// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Methods for extracting information from a symbol.</summary>
static class Extract
{
    /// <summary>Determines whether the span has the name of the interface with its nested generics.</summary>
    /// <param name="span">The name of the type.</param>
    /// <param name="symbols">The generics.</param>
    /// <returns>Whether the span has the name of the interface with its nested generics.</returns>
    public static bool MatchesGenerics(this in ReadOnlySpan<char> span, Span<TypeEntry> symbols) =>
        0 is var to && span.MatchesGenerics<TypeEntry>(symbols, ref to, x => x.Value);

    /// <summary>Determines whether the span has the name of the interface with its nested generics.</summary>
    /// <typeparam name="T">The type of symbol.</typeparam>
    /// <param name="span">The name of the type.</param>
    /// <param name="symbols">The generics.</param>
    /// <param name="to">The current index.</param>
    /// <param name="symbolic">The callback that converts each element into the symbol.</param>
    /// <returns>Whether the span has the name of the interface with its nested generics.</returns>
    public static bool MatchesGenerics<T>(
        this in ReadOnlySpan<char> span,
        in ReadOnlySpan<T> symbols,
        ref int to,
        Converter<T, ITypeSymbol> symbolic
    )
    {
        if (symbols.GetEnumerator() is var enumerator && !enumerator.MoveNext())
            return span.IsEmpty;

        for (var from = ++to; to < span.Length; to++)
            if (Step(span, ref to, symbolic, enumerator, ref from, out var matchesGenerics) is ControlFlow.Break)
                return matchesGenerics;

        return false;
    }

    /// <summary>Determines whether the span has the name of the interface with its nested generics.</summary>
    /// <typeparam name="T">The type of symbol.</typeparam>
    /// <param name="span">The name of the type.</param>
    /// <param name="symbolic">The callback that converts each element into the symbol.</param>
    /// <param name="to">The current index.</param>
    /// <param name="enumerator">The enumeration of generics.</param>
    /// <returns>Whether the span has the name of the interface with its nested generics.</returns>
    public static bool MatchesGenerics<T>(
        in ReadOnlySpan<char> span,
        Converter<T, ITypeSymbol> symbolic,
        ref int to,
        ref ReadOnlySpan<T>.Enumerator enumerator
    ) =>
        symbolic(enumerator.Current).TryGetTypeArguments(out var args) &&
        span.MatchesGenerics(args.AsSpan(), ref to, x => x);

    /// <summary>Gets the options.</summary>
    /// <param name="provider">The provider to get the options from.</param>
    /// <param name="symbol">The symbol to get its syntax tree from.</param>
    /// <returns>The options.</returns>
    public static AnalyzerConfigOptions GetOptions(
        this AnalyzerConfigOptionsProvider provider,
        [NotNullWhen(true)] ISymbol? symbol = null
    ) =>
        symbol?.DeclaringSyntaxReferences is [{ SyntaxTree: var syntax }, ..]
            ? provider.GetOptions(syntax)
            : provider.GlobalOptions;

    /// <summary>Gets all of the parameters and type arguments of the symbol.</summary>
    /// <param name="symbol">The symbol to get the parameters and type arguments from.</param>
    /// <returns>The <see cref="IEnumerable{T}"/> containing all underlying types.</returns>
    public static IEnumerable<ITypeSymbol> AllTypes(this ISymbol? symbol) =>
        symbol
           .AsTypeSymbolEnumerable()
           .Concat(symbol.Next());

    /// <summary>Gets the base type if it is has parents.</summary>
    /// <param name="symbol">The symbol to get the base type from.</param>
    /// <returns>The base type, if it has parents.</returns>
    public static INamedTypeSymbol? BaseTypeThatHasParents(this ITypeSymbol? symbol) =>
        symbol is { BaseType: { BaseType: not null } type } ? type : null;

    /// <summary>Gets the implicit interface of the provided argument.</summary>
    /// <param name="span">The string depicting a type member to extract the implicit interface from.</param>
    /// <returns>The implicit interface, or <see cref="ReadOnlySpan{T}.Empty"/>.</returns>
    public static ReadOnlySpan<char> ExplicitInterface(this ReadOnlySpan<char> span) => span[FindInterface(span)];

    /// <summary>Gets the unqualified name of the symbol.</summary>
    /// <remarks><para>
    /// This method calls <see cref="ISymbol.Name"/> and removes any and
    /// all qualifications, such as the name of a declaring interface.
    /// </para></remarks>
    /// <param name="symbol">The symbol to get the unqualified name from.</param>
    /// <returns>The unqualified name of the parameter <paramref name="symbol"/>.</returns>
    // ReSharper disable TailRecursiveCall
    public static SplitSpan<char> ToNamedParts(this ISymbol? symbol) =>
        symbol is not { Name: { } name } ? default :
        symbol is IEventSymbol or IFieldSymbol or IMethodSymbol or IPropertySymbol &&
        name.AsSpan().ExplicitInterface() is [.. var span, '.'] ? name.SplitAll(span) : new(name.AsSpan());
#pragma warning disable CA1508

    /// <summary>Finds the implicit interface of the provided argument.</summary>
    /// <param name="span">The string depicting a type member to extract the implicit interface from.</param>
    /// <returns>The range of characters in which the implicit interface exists.</returns>
    public static Range FindInterface(this in ReadOnlySpan<char> span)
    {
        if (span is ['.', ..])
            return default;

        int from = span switch
            {
                ['g', 'e', 't', '_', ..] => 4,
                ['s', 'e', 't', '_', ..] => 4,
                ['a', 'd', 'd', '_', ..] => 4,
                ['r', 'e', 'm', 'o', 'v', 'e', '_', ..] => 7,
                _ => 0,
            },
            to = 0,
            layer = 0;

        for (var i = from; i < span.Length; i++)
            if ((layer += span[i] switch
                {
                    '<' => 1,
                    '>' => -1,
                    _ => 0,
                }) is 0 &&
                span[i] is '.')
                to = i + 1;

        return to is 0 ? default : from..to;
    }
#pragma warning restore CA1508

    static bool SameName(this in ReadOnlySpan<char> name, ISymbol current) =>
        (current switch
        {
            IArrayTypeSymbol x => x.ElementType,
            IPointerTypeSymbol x => x.PointedAtType,
            _ => current,
        })
       .Name
       .AsSpan()
       .SequenceEqual(
            current switch
            {
                IArrayTypeSymbol => name.Nth(..^2),
                IPointerTypeSymbol => name.Nth(..^1),
                _ => name,
            }
        ) &&
        current switch
        {
            IArrayTypeSymbol => name.Nth(^2..) is "[]",
            IPointerTypeSymbol => name.Nth(^1) is '*',
            _ => true,
        };

    static bool SameName(this in SplitSpan<char> fullName, ISymbol current)
    {
        if (fullName.Single().SameName(current))
            return true;

        var symbols = SymbolSpan.New();
        symbols = Populate(current, symbols);

        if (symbols.GetEnumerator() is var e && !e.MoveNext() ||
            fullName.GetEnumerator() is var names && !names.MoveNext())
            return false;

        do
            if (!names.Current.SameName(e.Current))
                return false;
        while (e.MoveNext() && names.MoveNext());

        return true;
    }

    static ControlFlow Step<T>(
        in ReadOnlySpan<char> span,
        ref int to,
        Converter<T, ITypeSymbol> symbolic,
        ReadOnlySpan<T>.Enumerator enumerator,
        ref int from,
        out bool matchesGenerics
    )
    {
        Unsafe.SkipInit(out matchesGenerics);

        if (span[to] is not '<' and not ',' and not '>')
            return ControlFlow.Continue;

        var current = symbolic(enumerator.Current);

        if (!span[from..to].SplitAll([.. "."]).SameName(current))
        {
            matchesGenerics = false;
            return ControlFlow.Break;
        }

        from = to + 1;

        if (span[to] is ',' or '>' && current.TryGetTypeArguments(out _))
        {
            matchesGenerics = false;
            return ControlFlow.Break;
        }

        if (span[to] is ',' or '>' && !enumerator.MoveNext())
        {
            matchesGenerics = span[to++] is '>';
            return ControlFlow.Break;
        }

        if (span[to] is not '<')
            return ControlFlow.Continue;

        if (!MatchesGenerics(span, symbolic, ref to, ref enumerator))
        {
            matchesGenerics = false;
            return ControlFlow.Break;
        }

        from = to + 1;

        if (to >= span.Length || span[to] is not (',' or '>') || enumerator.MoveNext())
            return ControlFlow.Continue;

        matchesGenerics = span[to++] is '>';
        return ControlFlow.Break;
    }

    static IEnumerable<ITypeSymbol> AsTypeSymbolEnumerable(this ISymbol? symbol) =>
        symbol is ITypeSymbol i ? i.Yield() : Enumerable.Empty<ITypeSymbol>();

    static IEnumerable<ITypeSymbol> Next(this ISymbol? symbol) =>
        symbol
           .ToUnderlyingEnumerable()
           .Concat(symbol.ToParameterOwnedTypes())
           .SelectMany(AllTypes);

    static IEnumerable<ITypeSymbol> ToParameterOwnedTypes(this ISymbol? symbol) =>
        symbol switch
        {
            INamedTypeSymbol x => x.TypeArguments,
            IPropertySymbol x => x.Parameters.Select(x => x.Type),
            IMethodSymbol x => x.Parameters.Select(x => x.Type).Concat(x.TypeArguments),
            IFunctionPointerTypeSymbol { Signature: var x } => x.Parameters.Select(x => x.Type).Concat(x.TypeArguments),
            _ => Enumerable.Empty<ITypeSymbol>(),
        };

    static IEnumerable<ITypeSymbol> ToUnderlyingEnumerable(this ISymbol? symbol) =>
        symbol.ToUnderlying() is { } underlying ? underlying.Yield() : Enumerable.Empty<ITypeSymbol>();

    static Span<ISymbol> Populate(ISymbol current, Span<ISymbol> symbols)
    {
        symbols[^1] = current;

        for (var i = symbols.Length - 1; i > 0; i--)
        {
            if (symbols[i].ContainingSymbol is var containing && containing.IsNullOrGlobalNamespace())
            {
                symbols = symbols[i..];
                break;
            }

            symbols[i - 1] = containing;
        }

        return symbols;
    }
}
