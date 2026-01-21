# Cardputer uLisp Machine
A version of uLisp to convert the M5Stack Cardputer into a self-contained handheld Lisp computer. The files are as follows:

* **ulisp-cardputer.ino** - the uLisp source file, to be compiled and uploaded using the Arduino IDE.
* **Bignums.ino** - big numbers as a types. The **$** prefix is used on all functions.

## General Status of Build Size (on 2026-01-21 commit)

The free flash space is dependant on the kind of build partition scheme selected. The following 3 easy schemes work
with M5Launcher.

* About **5%** of memory is available after **Defualt** build.
* About **37%** free use **Minimal SPIFFS** partition scheme.
* About **61%** free use **Huge APP** (1 MB SPIFFS?).

Even when that's full it would still leave an extra 1.9 MB of app space free with a "custom" partitioning (**DOOM config** setting?).

The free **RAM** is 99404 Bytes (**31%** free) or about **22 k of lisp symbols**. **RAM** is most likely the limiting factor.

## Expansions

* **C Extension** in the binary program memory.
* **uLisp Extension** in the binary program memory **AND** also **uses up RAM** for lisp symbols.

