# Sympatch

Copy a directory via diff, using symlink for duplicate files.

## Usage

```
$ LANG=C diff -ruN src0/dir src1/dir > patch.diff
$ LANG=C dune exec -- bin/main.exe -p1 src0/dir dst/dir < patch.diff
```
