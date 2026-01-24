# Cardputer uLisp Machine
A version of uLisp to convert the M5Stack Cardputer into a self-contained handheld Lisp computer. The files are as follows:

* **ulisp-cardputer.ino** - the uLisp source file, to be compiled and uploaded using the Arduino IDE.
* **Bignums.ino** - big numbers as a types. The **$** prefix is used on all functions. (Merged).
* All the "M5Cardputer.h" dependancies with maybe some edits for uLisp. (Merged for ease of compile setup).
* **data** - SPIFFS files (keep it under 1 MB, not currently used).

You must add the `M5GFX` and `M5Unified` libraries to the Arduino project to satify all dependancies. As usual, the
`Sketch > Export compiled Binary` option places the binary in the project directory for use with M5Launcher.

## General Status of Build Size (on 2026-01-21 commit)

The free flash space is dependant on the kind of build partition scheme selected. The following 3 easy schemes work
with M5Launcher.

* About **5%** of memory is available after **Defualt** build.
* About **37%** free use **Minimal SPIFFS** partition scheme.
* About **61%** free use **Huge APP** (1 MB SPIFFS?).

Even when that's full it would still leave an extra 1.9 MB of app space free with a "custom" partitioning (**DOOM config** setting?).

The free **RAM** is 99404 Bytes (**31%** free) or about **22 k of lisp symbols**. **RAM** is most likely the limiting factor.

## Expansions (Implementation Options)

* **C Extension** in the binary program memory. Chain on something to **Bignums.ino** just like it does in turn.
* **uLisp Extension** in the binary program memory **AND** also **uses up RAM** for lisp symbols. Added a **LispLibrary.h** file.

## Things I Might Do (Not Everything, but Alot with Simple Function Choices)

- [ ] Interface UI Builder - make apps easier to make using GFX toolkit, perhaps some PROGMEM graphics images.
 - For example 5 buttons on a row at 42*9 box, 6 tiny chars with 2 pix border padding. 2 pix button horizontal outer padding
    and 5 pix horizontal gap extra (4 of these gaps).
- [ ] Improve Edit Experience - colors, cursor, help dialog.
- [X] Keyboard - swap `()` and `[]` as lisp. Also `\` with `|` for arcane CS reasons and CTRL "causing" a SHIFT for `^\` (not synthetic `ÈSC`).
- [ ] Maybe add some uses for `<backtick>`, `ctrl`, `alt`, `opt`and `fn`.
 - SI/SO, etc. Better done in keyboard handler? 
- [ ] `directory` - the card is 8 GB. Is `cd`reasonable?
- [ ] Color Keywords - improving mental parsing, and real pretty printing.
- [X] Bignums - from negative integer, subtract larger for possible error. BUG FIX.
- [ ] Bluetooth - maybe. Audio.
- [ ] Key Input -  maybe a non-blocking keyboard read.
- [ ] Sound - expand for more flexibility.
- [X] Numbers - a few more number theoretic functions.
- [ ] Calculus - a few more calculus functions.
- [ ] Solver - some kind of variable solver.
- [ ] Streams - maybe there's more. Audio.
- [ ] Other Types - for rational and complex. Choice of natural R or P representation.

## Other M5Carputer Projects

This list will expand as I place nice things I like.

* MiniAcid - ooh yes, tweeking for the ear.
* M5Launcher - a nice fine app as occasionally used!
* M5-Keyboard-Mouse - press that GO for keys or mouse. USB and Bluetooth.
* IR-Remote - apparently it does the lot.
* M5Mp3 - a gimmick, but 100 mp3 to play from SD.
* Wifi-Scan - clean show me the SSIDs.
* Porkchop - very entertaining.
* MicroHydra - yes, I'm using a Cardputer 1 for keys, but MicroPython.
