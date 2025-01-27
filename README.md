# ISL packaging repository

this repository only for packaging ISL. For simplify the build process, the interface need to generate localy. 

```sh
./autogen.sh
mkdir build
# export DYLD_LIBRARY_PATH="${DYLD_LIBRARY_PATH}:/Users/lisa/miniforge3/envs/ci/lib"  
./configure --prefix=`pwd`/build --with-int=imath --with-clang-prefix=/xxx/llvm-project/build/install # the custom libclang install path.
export CPATH="$(xcrun --show-sdk-path)/usr/include" # for macos
make
```
