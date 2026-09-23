# Mat Lang

This is an experimental project where i am creating my own programming language. This language serves no direct function besides teaching me to some compiler architecture.

The current version **doesn't** work.

## Prerequesites

You need to have a working installation ov LLVM 18.1.8 with the `llvm-config` binary.

If you are on Windows, you need the standart MSVC toolchain with `cmake` and `Ninja`.

Since standart, precompiled Windows LLVM Versions don't come with `llvm-config.exe` you may use the `build-llvm.ps1` powershell script to build it from source.
