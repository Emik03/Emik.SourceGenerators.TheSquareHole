// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

sealed record Scaffolder(string FileName, string Contents) : IEqualityComparer<Substitutes>
{
    /// <summary>Gets the instance in which both properties are empty strings.</summary>
    public static Scaffolder Empty { get; } = new("", "");

    /// <summary>Attempts to source generator from type substitutions.</summary>
    /// <param name="substitutes">The type substitutions to source generate.</param>
    /// <returns>
    /// The generated file name and contents based on contents in the parameter <paramref name="substitutes"/>.
    /// </returns>
    public static Scaffolder? Generate(IEnumerable<Substitutes> substitutes)
    {
        if (substitutes.OrderBy(x => x.Precedent()).ToSet(Empty).Where(x => x.CanImplementParents())
            is not ({ Type: var type } head, var tail))
            return null;

        var hintName = head.Type.HintName();
        var interfaces = tail.Prepend(head).Select(x => x.NameOfInterface()).ToSet();
        var content = Template(type, interfaces);
        return new(hintName, content);
    }

    /// <summary>Adds itself to the context.</summary>
    /// <param name="context">The <see cref="GeneratorExecutionContext"/> to add source code.</param>
    public void AddTo(GeneratorExecutionContext context) => context.AddSource(FileName, Contents);

    /// <summary>Adds itself to the context.</summary>
    /// <param name="context">The <see cref="SourceProductionContext"/> to add source code.</param>
    public void AddTo(SourceProductionContext context) => context.AddSource(FileName, Contents);

    /// <inheritdoc />
    bool IEqualityComparer<Substitutes>.Equals(Substitutes? x, Substitutes? y) =>
        x is null ? y is null : y is not null && !x.CanCoexistWith(y);

    /// <inheritdoc />
    int IEqualityComparer<Substitutes>.GetHashCode(Substitutes? obj)
    {
        const int Prime = 89;
        var hash = Prime;

        for (ISymbol? x = obj?.Interface; !x.IsNullOrGlobalNamespace(); x = x.ContainingSymbol)
            hash = unchecked(hash * Prime ^ StringComparer.Ordinal.GetHashCode(x.Name));

        return hash;
    }

    static string Inherit(IEnumerable<string> interfaces, int indent) =>
        interfaces.Conjoin($",\n{new string(' ', indent * 4)}");

    static string Template(ITypeSymbol type, IEnumerable<string> collection)
    {
        // ReSharper disable once PossibleMultipleEnumeration
        string WrapType(string acc, ITypeSymbol next) =>
            acc.Length is 0
                ? CSharp(
                    $$"""
                      partial {{next.Keyword()}} {{next.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}} : {{Inherit(collection, 1)}}
                      {
                      }
                      """
                )
                : CSharp(
                    $$"""
                      partial {{next.Keyword()}} {{next.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}}
                      {
                          {{acc.Replace("\n", "\n    ")}}
                      }
                      """
                );

        var typeDefinition = type
           .FindSmallPathToNull(x => x.ContainingType)
           .Aggregate("", WrapType);

        return $"{Header}{WrapInNamespace(typeDefinition, type.ContainingNamespace)}";
    }

    static string WrapInNamespace(string acc, INamespaceSymbol? name) =>
        (name is { IsGlobalNamespace: false }
            ? CSharp(
                $$"""
                  namespace {{name.ToDisplayString()}}
                  {
                      {{acc.Replace("\n", "\n    ")}}
                  }
                  """
            )
            : acc) +
        '\n';

    static string CSharp([StringSyntax("C#")] string x) => x;
}
