# Inquisitor
[![Quicklisp](http://quickdocs.org/badge/inquisitor.svg)](http://quickdocs.org/inquisitor/)

[![Build Status](https://travis-ci.org/t-sin/inquisitor.svg)](https://travis-ci.org/t-sin/inquisitor)
[![Circle CI](https://circleci.com/gh/t-sin/inquisitor.svg?style=svg)](https://circleci.com/gh/t-sin/inquisitor)
[![Coverage Status](https://coveralls.io/repos/t-sin/inquisitor/badge.svg?branch=master&service=github)](https://coveralls.io/github/t-sin/inquisitor?branch=master)

Encoding/end-of-line detection and external-format abstraction for Common Lisp.


> The Library is a sphere whose exact center is any one of its hexagons and whose circumference is inaccessible.
> -- ["The Library of Babel" by Jorge Luis Borges](http://hyperdiscordia.crywalt.com/library_of_babel.html)


## Goal

* encoding/end-of-line name abstraction
* encoding/end-of-line detection
* external-format abstraction
  * make external-format for each implementations
  * make external-format from byte-array, stream and pathname (with auto-detection)
  * abstract external-format of babel and flexi-stream
* many implementations (installable with [CIM](https://github.com/KeenS/CIM)) support
  * GNU CLISP
  * Embeddable Common Lisp
  * Steel Bank Common Lisp
  * Clozure CL
  * Armed Bear Common Lisp


## Installation

Get and install via quicklisp:

```
CL-USER> (ql:quickload :inquisitor)
```


## Usage

### Encoding detection

To detect encoding from stream, use `(inq:detect-encoding stream scheme)`.
About `scheme`, see `Encoding scheme`.

for example:

```lisp
CL-USER> (with-open-file (in #P"t/data/unicode/utf8.txt"
                          :direction :input
                          :element-type '(unsigned-byte 8))
           (inq:detect-encoding in :jp))
:UTF8
```

You can see the list of available encodings:

```lisp
CL-USER> (inq:available-encodings)
(:UTF8 :UCS-2LE :UCS-2BE :UTF16 :ISO-2022-JP :EUC-JP :CP932 :BIG5 :ISO-2022-TW
 :GB2312 :GB18030 :ISO-2022-CN :EUC-KR :JOHAB :ISO-2022-KR :ISO-8859-6 :CP1256
 :ISO-8859-7 :CP1253 :ISO-8859-8 :CP1255 :ISO-8859-9 :CP1254 :ISO-8859-5
 :KOI8-R :KOI8-U :CP866 :CP1251 :ISO-8859-2 :CP1250 :ISO-8859-13 :CP1257)
```

#### `Scheme`

`Scheme` is a hint to detect encoding.

It's mostly impossible to detect encoding universally, because there are two encoding such that use same byte sequences to represent other characters.
So, limitting target encodings has benefit to encoding detection.

Here, in inquisitor, **languages** are used to limit the encodings.
Where **languages** are, roughly speaking, writing systems used in anywhere arround the world.
Fixing language is equivalent to fixing possible characters. Becaus of which, encoding detection be slightly eazy.

Supported scheme (languages) is as follows:

- jp: japanese
- tw: taiwanese
- cn: chinese
- kr: korean
- ru: russian
- ar: arabic
- tr: turkish
- gr: greek
- hw: hebrew
- pl: polish
- bl: baltic


### Detecting end-of-line type

```lisp
(with-open-file (in "/path/to/utf8-lf.ja"
 :direction :input
 :element-type '(unsigned-byte 8))
  (inquisitor:detect-end-of-line in))
; => :LF
```

### Getting name on your implementation

```lisp
(inquisitor.names:name-on-impl :cp932)
; => #<ENCODING "CP932" :UNIX>  ; on CLISP
; => :WINDOWS-CP932  ; on ECL
; => :SHIFT_JIS  ; on SBCL
; => :WINDOWS-31J  ; on CCL
; => :|X-MS932_0213|  ; on ABCL
```

#### If you want to know eol is available on your implementation

Use `inquisitor.eol:eol-available-p`.


### Making external-format implementation independently

```lisp
(inquisitor:make-external-format
  :utf8 ; implementation independent name of UTF-8
  :lf) ; implementation independent name of LF
; => :UTF-8  ; on SBCL
; => #<EXTERNAL-FORMAT :CP932/:DOS #xxxxxxxxxxx>  ; on CCL
```

#### Auto detecting and making external-format, from vector, stream and pathname

In case of vector (on CCL):

```lisp
(inquisitor:detect-external-format
  (encode-string-to-octets "公的な捜索係、調査官がいる。
わたしは彼らが任務を遂行しているところを見た。")
  :jp)
; => #<EXTERNAL-FORMAT :UTF-8/:UNIX #xxxxxxxxxx>
```

In case of stream (on CCL):

```lisp
(with-open-file (in "/path/to/utf8-lf.ja"
 :direction :input
 :element-type '(unsigned-byte 8))
   (inquisitor:detect-external-format in :jp)
; => #<EXTERNAL-FORMAT :UTF-8/:UNIX #xxxxxxxxxx>
```

In case of pathname (on CCL):

```lisp
(inquisitor:detect-external-format #P"/path/to/utf8-lf.ja" :jp)
; =># <EXTERNAL-FORMAT :UTF-8/:UNIX #xxxxxxxxxx>
```

## Author

* [Shiro Kawai](https://github.com/shirok) - original code of encoding detection for [Gauche](https://github.com/shirok/Gauche/tree/master/ext/charconv)
* [Masayuki Onjo](http://lispuser.net/index) - [porting](http://lispuser.net/commonlisp/japanese.html#sec-2.1) from Gauche to Common Lisp
* [zqwell](https://github.com/zqwell) - [porting](https://github.com/zqwell/guess) multilingual encoding detection from [libguess](https://github.com/kaniini/libguess)
* Shinichi Tanaka (shinichi.tanaka45@gmail.com) - adding end-of-line detection and wrapping external-format


## Copyright

Copyright (c) 2000-2007 Shiro Kawai (shiro@acm.org)  
Copyright (c) 2007 Masayuki Onjo (onjo@lispuser.net)  
Copyright (c) 2011 zqwell (zqwell@gmail.com)  
Copyright (c) 2015 Shinichi Tanaka (shinichi.tanaka45@gmail.com)


## License

Licensed under the MIT License.
