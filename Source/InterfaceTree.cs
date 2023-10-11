// SPDX-License-Identifier: MPL-2.0
namespace Emik.SourceGenerators.TheSquareHole;

/// <summary>Represents multiple interfaces as a tree of inheritance.</summary>
/// <param name="symbol">The current interface.</param>
sealed class InterfaceTree(INamedTypeSymbol symbol) : ICustomFormatter
{
    SmallList<InterfaceTree> _children, _parents;

    /// <summary>Gets the children recursively.</summary>
    public IEnumerable<InterfaceTree> ChildrenRecursively => GetFlattenedEnumerator(x => x.Children);

    /// <summary>Gets the parents recursively.</summary>
    public IEnumerable<InterfaceTree> ParentsRecursively => GetFlattenedEnumerator(x => x.Parents);

    /// <summary>Gets the current interface.</summary>
    public INamedTypeSymbol Node { get; } = symbol;

    /// <summary>Gets the children.</summary>
    public ref SmallList<InterfaceTree> Children => ref _children;

    /// <summary>Gets the parents.</summary>
    public ref SmallList<InterfaceTree> Parents => ref _parents;

    /// <summary>Creates the tree from the set of interfaces.</summary>
    /// <param name="interfaces">The interfaces to create a tree from.</param>
    /// <returns>The list of trees based on the hierarchy structure within <paramref name="interfaces"/>.</returns>
    public static IList<InterfaceTree> From(IEnumerable<INamedTypeSymbol> interfaces)
    {
        List<InterfaceTree> accumulator = new();

        foreach (var next in interfaces)
            Step(ref accumulator, next);

        return accumulator;
    }

    /// <summary>Deconstructs this instance.</summary>
    /// <param name="node">The reference to assign to the property <see cref="Node"/>.</param>
    /// <param name="children">The reference to assign to the property <paramref name="children"/>.</param>
    public void Deconstruct(out INamedTypeSymbol node, out ReadOnlySpan<SmallList<InterfaceTree>> children)
    {
        node = Node;
        children = Span.Ref(ref _children);
    }

    /// <inheritdoc />
    public override string ToString() =>
        $"{symbol.Stringify()} has {_children.Count.Conjugate("child", "ren")} and {_parents.Count.Conjugate("parent")}";

    /// <summary>Gets the enumeration recursively based on the function.</summary>
    /// <param name="func">The function to use for each enumeration.</param>
    /// <returns>The enumeration of <see cref="InterfaceTree"/> from this instance.</returns>
    public IEnumerable<object> GetEnumerator(Func<InterfaceTree, IEnumerable<InterfaceTree>> func) =>
        func(this).Select(x => x.GetEnumerator(func)).Prepend(this as object);

    /// <summary>Gets the enumeration recursively based on the function.</summary>
    /// <typeparam name="T">The type of return value that represents enumeration.</typeparam>
    /// <param name="func">The function to use for each enumeration.</param>
    /// <returns>The enumeration of <see cref="InterfaceTree"/> from this instance.</returns>
    public IEnumerable<InterfaceTree> GetFlattenedEnumerator<T>(Func<InterfaceTree, T> func)
        where T : IEnumerable<InterfaceTree>
    {
        yield return this;

        foreach (var x in func(this).SelectMany(x => x.GetFlattenedEnumerator(func)))
            yield return x;
    }

    /// <inheritdoc />
    string ICustomFormatter.Format(string format, object arg, IFormatProvider formatProvider) => ToString();

    static void Step<T>(ref T children, INamedTypeSymbol next, InterfaceTree? parent = null)
        where T : IList<InterfaceTree>
    {
        for (var i = 0; i < children.Count; i++)
            if (HasMutated(ref children, next, i) is ControlFlow.Break)
                return;

        var child = Spawn(next, parent);
        children.Add(child);
    }

    static ControlFlow HasMutated<T>(ref T children, INamedTypeSymbol next, int i)
        where T : IList<InterfaceTree>
    {
        var child = children[i];
        var node = child.Node;

        if (next.AllInterfaces.Any(node.IsSameOverload))
        {
            Step(ref child._children, next, child);
            return ControlFlow.Break;
        }

        if (!node.AllInterfaces.Any(next.IsSameOverload))
            return ControlFlow.Continue;

        children[i] = Adopt(child, next);
        return ControlFlow.Break;
    }

    static InterfaceTree Adopt(InterfaceTree child, INamedTypeSymbol next)
    {
        InterfaceTree parent = new(next)
        {
            _children = child,
        };

        child._parents.Add(parent);
        return parent;
    }

    static InterfaceTree Spawn(INamedTypeSymbol next, InterfaceTree? parent)
    {
        InterfaceTree child = new(next)
        {
            _parents = parent?.AsSmallList() ?? default,
        };

        return child;
    }
}
