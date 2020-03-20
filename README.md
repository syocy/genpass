# genpass

## Install

First, Install GHC and Cabal (e.g. [ghcup](https://www.haskell.org/ghcup/))

```shell
$ git clone <this repository>
$ cd genpass
$ cabal v2-install
```

## Usage

### Generate Alpha-Num Password

```shell
$ genpass gen
6jKyGbWh
```

### Generate 3-Times 40-Length Password

```shell
$ genpass gen -n3 -l40
U0zvC9H32OYNUg4Ow7bzlLEmwXZEB0lvs7uyCjzn
ULALESgxglcyqtca5uurZcLUpZCJ5wJzAa0i9wce
QOqyG0x4zB78d7cLWTs3WSTGL1GAQxXUm1PgRDnP
```

### Generate Upper-Case Passwords

```shell
$ genpass gen -n3 -l8 --no-digit --no-alpha-lower
EVFSQIEW
UEPQRAIP
XFNVPQRI
```

### Generate Passwords with Specified Symbols

```shell
$ genpass gen -n3 -l8 --symbol=/@!+-=_/
-vih++2w
T2sGBYQ6
@iZSRnJg
```

### Check Password Length

```shell
$ genpass check -l12 "abcd1234"
abcd1234 is shorter than 12
```

### Check Password Character Set Kinds

```shell
$ genpass check --kind=3 "abcd1234"
abcd1234 contains too few kinds of chars
```

### Advanced: Generate Passwords with Japanese Kana Letters

```shell
$ genpass gen -n3 -l8 --hiragana --katakana
OジとドぃWワヹ
ワつォWげnぞj
セぎヹゐヹシヱ6
```
