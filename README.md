# Mercury-Zigzag

`mercury_zigzag` is a [Mercury](http://www.mercurylang.org) library for
encoding signed Mercury integer types using
[Zigzag](https://en.wikipedia.org/wiki/Variable-length_quantity#Zigzag_encoding)
encoding.

## License

`mercury_zingzag` is licensed under a simple 2-clause BSD style license.
See the file [COPYING](COPYING) for details.

## Installation

Check the values in the file [Make.options](Make.options) to see if they agree
with your system, then do:

```
    make install
```

You can also override values in [Make.options](Make.options) on the command
line, for example

```
    make INSTALL_PREFIX=/foo/bar install
```

causes the library to be installed in the directory `/foo/bar`.

## Author

Julien Fischer <juliensf@gmail.com>
