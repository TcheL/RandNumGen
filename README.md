# RandNumGen

Random Number Generator for specific statistic distribution, include uniform, normal, etc.

## License

[The MIT License](http://tchel.mit-license.org)

## Compile

Although the statically compiled binary executable file **rngen** is available on Linux platform, you can compile the source file **rngen.F90** on your system as follow:

```shell
$ gfortran -cpp rngen.F90 -o rngen
```

or

```shell
$ ifort -cpp rngen.F90 -DIFORT -o rngen
```

## Examples

Help info. can be obtained by `./rngen -h`:

```shell
Usage: rngen [-v IsVerbose] [-s IsSilent] [ -uni unia,unib | -nor norMu,norSigma ] [-m MulprScale] [-i IncrmValue] [-d Len2Dim]
  [IsVerbose     ]: print verbose warning and help information (T) or not (F).
  [IsSilent      ]: print configure parameter (F) or not (T).
  [unia,unib     ]: uniform distribution on the interval [unia, unib].
  [norMu,norSigma]: normal distribution with mean norMu and variance norSigma.
  [MulprScale    ]: original random number multiplied by MulprScale to scale.
  [IncrmValue    ]: intermediary random number added by IncrmValue to shift.
  [Len2Dim       ]: dimension lengths of 2-D output array: dim1_len,dim2_len
```

As shown above, to generate a $5\times 3$ array of uniform-distributed random number on the interval $[10, 20]$, we can run:

```shell
$ ./rngen -uni 1,2 -m 10 -d 5,3
```

Similarly, to generate a $1\times 20$ array of normal-distributed random number with $\mu=10$ and $\sigma=1$, we can run:

```shell
$ ./rngen -nor 10,1 -d 1,20
```

## Author

Tche LIU, <seistche@gmail.com>, USTC