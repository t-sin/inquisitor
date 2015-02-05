# Inquisitor

Encoding/end-of-line detecter and wrapper of external-format for Common Lisp.


> "The Library is a sphere whose exact center is any one of its hexagons and whose circumference is inaccessible."

[_"The Library of Babel"_ by _Jorge Luis Borges_](http://hyperdiscordia.crywalt.com/library_of_babel.html)


## Goal

* encoding/end-of-line detection
* external-format abstraction
  * make external-format for each implementations
  * make external-format from byte-array, stream and pathname (with auto-detection)
* many implementations (installable with [CIM](https://github.com/KeenS/CIM)) support
  * GNU CLISP
  * Embeddable Common Lisp
  * Steel Bank Common Lisp
  * Clozure CL
  * Armed Bear Common Lisp


## Usage

Put in ASDF-path and type your REPL:

    (require :inquisitor)


## Author

* [Shiro Kawai](https://github.com/shirok) - original code of encoding detection for [Gauche](https://github.com/shirok/Gauche/tree/master/ext/charconv)
* [Masayuki Onjo](http://lispuser.net/index) - [porting](http://lispuser.net/commonlisp/japanese.html#sec-2.1) from Gauche to Common Lisp
* [zqwell](https://github.com/zqwell) - [porting](https://github.com/zqwell/guess) multilingual encoding detection from [libguess](https://github.com/kaniini/libguess)
* gray (shinichi.tanaka45@gmail.com) - adding end-of-line detection and wrapping external-format


## Copyright

Copyright (c) 2000-2007 Shiro Kawai (shiro@acm.org)  
Copyright (c) 2007 Masayuki Onjo (onjo@lispuser.net)  
Copyright (c) 2011 zqwell (zqwell@gmail.com)  
Copyright (c) 2015 gray (shinichi.tanaka45@gmail.com)
