# Sympatch

Copy a directory via diff, using symlink for duplicate files.

## Usage

```
$ diff -ruN src0/dir src1/dir > patchfile
$ sympatch -p 1 [-opt] src0/dir dst/dir < patchfile
```

## Motivation

Suppose you have one (original) directory `dir` and many other directories `dirN`
slightly different from `dir`, though most files are the same as those in `dir`.
To avoid duplicate files, you can use symlinks from files in `dirN` to those in `dir`.
But making such symlinks by hand is tedious.

This is where Sympatch comes in. Sympatch creates such symlinks from a patch file.
That is, you first create a unified-formatted patch file between `dir` and `dirN`
by `diff -urN dir dirN > patchfile`. Then, you can use `patchfile` as Symlink's input
to make a new duplicate-free `dirN` directory constructed with symlinks.

```sh
# The source directory `dir`
$ tree dir
dir
├── a
│   ├── b
│   ├── d
│   └── e
└── c

1 directory, 4 files

# One of (many) other directories (dir1)
$ tree dir1
dir1
├── a
│   ├── d
│   └── e
└── c

1 directory, 3 files

# Create a patchfile. The file a/b is removed and file c is changed,
# but a/d and a/e are intact.
$ diff -ruN dir dir1 | tee patchfile
diff -ruN dir/a/b dir1/a/b
--- dir/a/b	2022-12-01 00:08:10.519212252 +0900
+++ dir1/a/b	1970-01-01 09:00:00.000000000 +0900
@@ -1 +0,0 @@
-foo
diff -ruN dir/c dir1/c
--- dir/c	2022-12-01 00:08:12.975213879 +0900
+++ dir1/c	2022-12-01 00:09:02.431244340 +0900
@@ -1 +1 @@
-bar
+baz

# Apply patch to make dir1_copied
$ sympatch -p 1 dir dir1_copied < patchfile

# It seems dir1_copied has the same content as dir1
$ diff -r dir1 dir1_copied

# But it uses symlinks to avoid duplicate files.
$ tree dir1_copied
dir1_copied
├── a
│   ├── d -> ../../dir/a/d
│   └── e -> ../../dir/a/e
└── c

1 directory, 3 files
```

## Build

```
$ dune build bin/main.exe
```

Type `dune exec bin/main.exe --` instead of `sympatch`.

## License

MIT.
