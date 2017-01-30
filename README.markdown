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
This returns **implementation independent** encoding name.
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

#### Encoding scheme

Encoding scheme is a hint to detect encoding.

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
- ru: russian (latin-5)
- ar: arabic (latin-6)
- tr: turkish (latin-9)
- gr: greek (latin-7)
- hw: hebrew (latin-8)
- pl: polish (latin-2)
- bl: baltic (latin-7)


### End-of-line type detection

If you want to know end-of-line (line break) type, use `(inq:detect-end-of-line stream)`.
This returns **implementation independent** end-of-line name.

```Lisp
CL-USER> (with-open-file (in "t/data/ascii/ascii-crlf.txt"
                             :direction :input
                             :element-type '(unsigned-byte 8))
           (inquisitor:detect-end-of-line in))

:CRLF
```

### Names on each implementation

If you want to know implementation dependent name of encodings or eol type, use `(inq:independent-name name)`.
Returned value can be used as external-format, or its part.

```lisp
CL-USER> (inq:independent-name :cp932)
#<ENCODING "CP932" :UNIX>  ; on CLISP
:WINDOWS-CP932  ; on ECL
:SHIFT_JIS  ; on SBCL
:WINDOWS-31J  ; on CCL
:|X-MS932_0213|  ; on ABCL
```

#### Eol

If you want to know eol is available on your implementation, use `(inq:eol-available-p)`.

```lisp
CL-USER> (inq:eol-available-p)
NIL  ; on SBCL
```

### Make external-format

To make external-format from impl independent names, use `(inq:make-external-format enc eol)`.

In SBCL and CCL, same code returns different value.

On SBCL:

```lisp
CL-USER> (let* ((file #P"t/data/ja/sjis.txt")
                (enc (inq:detect-encoding file :jp))
                (eol (inq:detect-end-of-line file)))
           (inq:make-external-format enc eol))
:SHIFT_JIS
```

On CCL:

```lisp
CL-USER> (let* ((file #P"t/data/ja/sjis.txt")
                (enc (inq:detect-encoding file :jp))
                (eol (inq:detect-end-of-line file)))
           (inq:make-external-format enc eol))
#<EXTERNAL-FORMAT :WINDOWS-31J/:UNIX #x302001C574CD>
```

#### External-format detection

Inquisitor provides external-format detection method.
It detects encoding and eol style, then make external-format from these.
It can use with vector, byte stream and pathname.

Let's see examples with CCL.


##### From vector

```lisp
CL-USER> (inq:detect-external-format
          (encode-string-to-octets "公的な捜索係、調査官がいる。
わたしは彼らが任務を遂行しているところを見た。")
          :jp)
#<EXTERNAL-FORMAT :UTF-8/:UNIX #x30200046719D>
```

##### From stream

```lisp
CL-USER> (with-open-file (in "t/data/unicode/utf8.txt"
                             :direction :input
                             :element-type '(unsigned-byte 8))
           (inq:detect-external-format in :jp))
#<EXTERNAL-FORMAT :UTF-8/:UNIX #x30200046719D>
```

##### From pathname

```lisp
CL-USER> (inq:detect-external-format #P"t/data/unicode/utf8.txt" :jp)
#<EXTERNAL-FORMAT :UTF-8/:UNIX #x30200046719D>
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
