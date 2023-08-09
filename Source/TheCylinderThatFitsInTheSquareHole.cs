// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>The source generator that implements structural typing.</summary>
[CLSCompliant(false), Generator]
public sealed class TheCylinderThatFitsInTheSquareHole : ISourceGenerator
{
    /// <inheritdoc />
    void ISourceGenerator.Execute(GeneratorExecutionContext context) =>
#if DEBUG
        new BadLogger().Try(Go, context).Dispose();
#else
        Go(context);
#endif

    /// <inheritdoc />
    void ISourceGenerator.Initialize(GeneratorInitializationContext context) { }

    static void Go(GeneratorExecutionContext context)
    {
        // ReSharper disable once MissingIndent
        if (context is not
        {
            AnalyzerConfigOptions: var config,
            Compilation: { Assembly: var assembly, GlobalNamespace: var global },
        })
            return;

        Substitutes.Clear();

        var trees = global
           .GetAllMembers()
           .OfType<INamedTypeSymbol>()
           .Where(IncludedSyntaxNodeRegistrant.IsInterface)
           .Where(x => x.IsFullyAccessible(assembly))
           .Then(InterfaceTree.From);

        // This is the hotspot. Optimizations should occur within this call.
        void Next(INamedTypeSymbol type) =>
            trees.SelectMany(x => Substitutes.From(type, x, config)).Then(Scaffolder.Generate)?.AddTo(context);

        var types = assembly
           .GetAllMembers()
           .OfType<INamedTypeSymbol>()
           .Omit(SymbolPredicates.IsIgnored)
           .Omit(IncludedSyntaxNodeRegistrant.IsInterface)
           .Where(SymbolPredicates.IsExtendable);

        if (config.GlobalOptions.Then(Config.From).EnableConcurrency)
            Parallel.ForEach(types.AsParallel(), Next);
        else
            types.Lazily(Next).Enumerate();
    }
}
