
# Tom

[![Hackage](https://img.shields.io/hackage/v/tom.svg)](https://hackage.haskell.org/package/tom)
[![Build status](https://travis-ci.org/aelve/tom.svg)](https://travis-ci.org/aelve/tom)
[![Coverage](https://img.shields.io/coveralls/aelve/tom.svg)](https://coveralls.io/github/aelve/tom)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/tom/blob/master/LICENSE)

This is a minimalistic tool for reminders:

* you enter time (which can be recurring)
* you enter text
* at that time a reminder pops up and you can delay it or turn it off

## Building instructions

```
$ stack install gtk2hs-buildtools
$ stack build
```

If Stack complains about not finding GHC, do `stack setup` first.
