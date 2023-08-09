// SPDX-License-Identifier: MPL-2.0
using A = System.Int32;

namespace Emik.SourceGenerators.TheSquareHole.Generated.Tests;
#pragma warning disable 219, CA1819, IDE0059, MA0025, MA0110, RCS1085

public partial class Cylinder<T1, T2>(T1 second)
{
    public int First => throw new UnreachableException();

    public T1 Second => second;
}

interface ISquare<out T1, out T2>
{
    T1 First { get; }

    T2 Second { get; }
}
