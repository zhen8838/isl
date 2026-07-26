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

CI generates the language bindings itself: a first job installs clang,
runs `extract_interface` over the isl headers to produce
`interface/isl.py.core` and `interface/Interop.cs`, and hands them to the
wheel builds. Changing a header is therefore enough; regenerating locally is
optional. The copies committed here are what a source install uses, and CI
warns when they no longer match what it generated.

To regenerate them by hand you need clang:

```sh
./configure --with-clang=system --with-int=imath
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