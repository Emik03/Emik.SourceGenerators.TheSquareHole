# Emik.SourceGenerators.TheSquareHole

[![NuGet package](https://img.shields.io/nuget/v/Emik.SourceGenerators.TheSquareHole.svg?logo=NuGet)](https://www.nuget.org/packages/Emik.SourceGenerators.TheSquareHole)
[![License](https://img.shields.io/github/license/Emik03/Emik.SourceGenerators.TheSquareHole.svg?style=flat)](https://github.com/Emik03/Emik.SourceGenerators.TheSquareHole/blob/main/LICENSE)

<blockquote>
    <img src="https://raw.githubusercontent.com/Emik03/Emik.SourceGenerators.TheSquareHole/main/Images/icon.png" alt="icon.png" width="50"/>
    &emsp;
    <a href="https://youtu.be/Nz8ssH7LiB0?t=22">
       "And up next, a cylinder. Hmm, I think that goes in... the square hole!"
    </a>
</blockquote>

Adds structural typing to C#. Made as a celebration for my 20th birthday on August 8th 2023.

This project has a dependency to [Emik.Morsels](https://github.com/Emik03/Emik.Morsels), if you are building this project, refer to its [README](https://github.com/Emik03/Emik.Morsels/blob/main/README.md) first.

---

- [Why](#why)
- [How](#how)
- [Configure](#configure)
- [Contribute](#contribute)
- [License](#license)

---

## Why

Despite the presentation of the project, the project itself is treated very seriously and addresses a real-world problem.

When creating software, you are encouraged to write code that is easily extensible and reusable. Interfaces are a fantastic way of achieving this, often allowing you to reuse methods that take said interface as a parameter. Part of the problem however is having to juggle the signatures of each interface in your head.

If you are creating a type specifically to implement an interface, this isn't much of a problem, but if you want to maximize the re-usability of a type, particularly if you make a type whose purpose is fairly generic, then you may often sit thinking about each interface you may consider adding.

Languages like Scala solve this issue using Structural Typing. If a type has the same declaring members as an interface, then it derives it.

## How

This source generator looks at every interface accessible from your assembly and determines whether it is able to implement the interface.

Empty interfaces, alongside attributes marked with [`ObsoleteAttribute`](https://learn.microsoft.com/en-us/dotnet/api/system.obsoleteattribute?view=net-7.0) are **not considered part of the search**, as these interfaces tend to function more like attributes, and should therefore be opt-in.

If the interface contains generics, then the source generator performs **Type Substitution**: It considers every type declared within any type, including itself. For instance, take a look at the following type:

```csharp
public class A(int i);
```

The type `A` declares a method with a parameter `int`, with nothing else. Therefore the candidates are `int` and `A`.

This results in the source generator being extremely flexible, and accounts for every possible implementation. For instance:

```csharp
public partial record Cylinder<T1, T2>(T1 Second)
{
    public int First => 0;
}

interface ISquare<out T1, out T2>
{
    T1 First { get; }

    T2 Second { get; }
}
```

...generates...

```csharp
// <auto-generated/>
#nullable enable
partial record Cylinder<T1, T2> : global::ISquare<int, T1>,
    global::System.Numerics.IEqualityOperators<global::Cylinder<T1, T2>, global::Cylinder<T1, T2>, bool>
{
}
```

The hard upper limit for type substitution with generics are exactly **3** of them. This does not include generics which are self-constrained.

While this analyzer does perform a lot of tricks to squeeze performance, type substituion may be still too expensive on your machine. Refer to [Configure](#configure) in that case.

## Configure

Use `.editorconfig`/`.globalconfig` to configure this source generator:

---

| Option   | `the_square_hole_enable_concurrency`                                                                                                                                                                                                                                                                                                  |
|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Summary  | Determines whether to enable concurrency for inspections.                                                                                                                                                                                                                                                                             |
| Remarks  | Concurrency is only faster in large projects due to overhead in initializing concurrent behavior, hence why it's disabled by default.                                                                                                                                                                                                 |
| Obsolete | This option is deprecated starting from `1.1` and onwards due to the migration of the favorable [`IIncrementalGenerator`](https://learn.microsoft.com/en-us/dotnet/api/microsoft.codeanalysis.iincrementalgenerator) over [`ISourceGenerator`](https://learn.microsoft.com/en-us/dotnet/api/microsoft.codeanalysis.isourcegenerator). |
| Type     | `bool`                                                                                                                                                                                                                                                                                                                                |
| Default  | `false`                                                                                                                                                                                                                                                                                                                               |

---

| Option  | `the_square_hole_include_nullability`                                                                                                                                                                                                                                    |
|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Summary | Determines whether or not to include nullability as a restriction.                                                                                                                                                                                                       |
| Remarks | `A(string s)` and `A(string? s)` are equal interface-wise despite the unequal signature metadata. This can however violate the contract of the interface, and you will get the suggestion to change the signature, which would be a breaking feature in an existing API. |
| Type    | `bool`                                                                                                                                                                                                                                                                   |
| Default | `false`                                                                                                                                                                                                                                                                  |

---

| Option  | `the_square_hole_include_parameter_name`                                                                                                                                                                                 |
|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Summary | Determines whether or not to include parameter names as a restriction.                                                                                                                                                   |
| Remarks | `A(int a)` and `A(int b)` are equal interface-wise despite the unequal parameter naming. Some analyzers encourage renaming parameters when such a scenario occurs, which would be a breaking feature in an existing API. |
| Type    | `bool`                                                                                                                                                                                                                   |
| Default | `false`                                                                                                                                                                                                                  |

---

| Option  | `the_square_hole_max_substitution_depth`                                                        |
|---------|-------------------------------------------------------------------------------------------------|
| Summary | Determines the maximum number of type substitutions allowed for a given interface.              |
| Remarks | Lower = faster, higher = better inference of generics. Lower it if you face performance issues. |
| Type    | `0..=3`                                                                                         |
| Default | `3`                                                                                             |

---

## Contribute

Issues and pull requests are welcome to help this repository be the best it can be.

## License

This repository falls under the [MPL-2 license](https://www.mozilla.org/en-US/MPL/2.0/).
