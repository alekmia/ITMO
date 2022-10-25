# shuf command line utility

## Name
shuf -- generate random permutations

## Synopsis

```bash
shuf [OPTIONS] [FILE]
```

## Description
Write a random permutation of the input lines to standard output.

Options:
* `-i lo-hi, --input-range=lo-hi` - Act as if input came from a file containing the range of unsigned decimal integers lo…hi, one per line
* `-n count, --head-count=count` - Output at most count lines. By default, all input lines are output
* `-r, --repeat` - Repeat output values, that is, select with replacement. With this option the output is not a permutation of the input; instead, each output line is randomly chosen from all the inputs. This option is typically combined with `--head-count`; if `--head-count` is not given, shuf repeats indefinitely

### Example
```bash
$ cat e.txt
Aaa
BbBb
Bbbb
123
Aaa
$ shuf e.txt
123
Aaa
Bbbb
Aaa
BbBb
$ shuf -n 2 e.txt
Bbbb
Aaa
$ shuf -r -n 10 e.txt
BbBb
123
Bbbb
Aaa
BbBb
Aaa
123
Aaa
Aaa
BbBb
$ shuf -i 1-3
3
1
2
```
