// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>The source generator that implements structural typing.</summary>
#pragma warning disable RS1038
[CLSCompliant(false), Generator]
#pragma warning restore RS1038
public sealed class TheCylinderThatFitsInTheSquareHole : IIncrementalGenerator
{
    /// <inheritdoc />
    void IIncrementalGenerator.Initialize(IncrementalGeneratorInitializationContext context)
    {
        var owned = context
           .SyntaxProvider
           .AgainstAttributeWithMetadataName(Of<AttributeGenerator>(), Is<BaseTypeDeclarationSyntax>, Transform)
           .Filter();

        var interfaces = context.CompilationProvider.Select(InterfaceTrees);
        var config = context.AnalyzerConfigOptionsProvider.Select(Config.From);
        var provider = owned.Combine(interfaces).Combine(config);
        context.RegisterSourceOutput(provider, Go);
    }

    static void Go(SourceProductionContext context, ((INamedTypeSymbol, IList<InterfaceTree>), Config) tuple)
    {
        var ((named, trees), config) = tuple;
        trees.SelectMany(x => Substitutes.From(named, x, config)).Then(Scaffolder.Generate)?.AddTo(context);
    }

    static IList<InterfaceTree> InterfaceTrees(Compilation compilation, CancellationToken token) =>
        compilation
           .GlobalNamespace
           .GetAllMembers()
           .OfType<INamedTypeSymbol>()
           .Where(IncludedSyntaxNodeRegistrant.IsInterface)
           .Where(x => x.IsFullyAccessible(compilation.Assembly))
           .Then(InterfaceTree.From);

    static INamedTypeSymbol? Transform(SyntaxNode node, ISymbol symbol, SemanticModel _, CancellationToken token) =>
        symbol is INamedTypeSymbol named && !named.IsInterface() && named.IsExtendable() && node.IsFirst(symbol, token)
            ? named
            : null;
}
