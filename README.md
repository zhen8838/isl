# ISL packaging repository

this repository only for packaging ISL.

# Releasing

Push a tag. That is the whole procedure.

```sh
git tag v0.1.9
git push origin v0.1.9
```

The version of the wheels is the tag name, so no file in this repository
carries it and there is nothing to edit before tagging. Anything built from
an untagged commit is versioned `0.0.0.dev0+g<sha>` and is not uploaded.

`interface/isl.py.core` and `interface/Interop.cs` are produced from the isl
headers by `interface/extract_interface`, a clang tool, and are not kept in
the repository. A first CI job builds that tool, runs it and hands the result
to the wheel builds, so changing a header is all a change takes.

Building from a source checkout means generating them first, which needs
clang 17 (isl 0.26 uses a `SourceManager::createFileID` overload that clang 18
removed):

```sh
./configure --with-clang-prefix=/path/to/llvm-17 --with-int=imath
make interface/isl.py.core interface/Interop.cs
```

# Todo List

- [] python wheels on windows AMD64
- [x] python wheels on linux x86_64
- [x] python wheels on mac arm64
- [] python wheels on mac x86_64
- [] make isl_*_list iterable in python interface
- [x] export remove_map_if in python interface
- [] export isl_pw_qpolynomial in python interface
- [] fix export isl_printer_to_str in python interface
- [] fix codegen test segment fault
- [] optimize get_xx method as property in python interface
- [] ⚠️ fix multi_pw_aff and pw_multi_aff confuse in python