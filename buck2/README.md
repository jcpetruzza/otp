# Using buck2

The version of buck2 to use is pinned via a dotslash distribution. As
a first step, ensure you are using the pinned version of buck:

```
$ export PATH=$ERL_TOP/buck2/bin`
```

## Alias modifiers

* For emulator flavor

```
-m emu
-m jit
```

* For emulator types (multiple can be given)
```
-m asan
-m debug
-m frmptr
-m gcov
-m gprof
-m icount
-m lcnt
-m valgrind
```
