# Cardputer uLisp Machine
A version of uLisp to convert the M5Stack Cardputer into a self-contained handheld Lisp computer. The files are as follows:

* **ulisp-cardputer.ino** - the uLisp source file, to be compiled and uploaded using the Arduino IDE.
* **Bignums.ino** - big numbers as a types. The **$** prefix is used on all functions. (Merged).
* All the "M5Cardputer.h" dependancies with maybe some edits for uLisp. (Merged for ease of compile setup).
* **uLisp.lang** - a syntax file for **uLisp** to make it easier to use an editor like **Xed** from **Linux Mint**.
* **tokyo-night.xml** - a nice MIT licenced theme for **Xed**.
* **data** - SPIFFS files (keep it under 1 MB, not currently used).

You must add the `M5Cardputer`, `M5GFX` and `M5Unified` libraries to the Arduino project to satify all dependancies. As usual, the
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

See **LispLibrary.h** for details of the expanding set of lisp additions, along with some copied from the uLisp documentation.
I'm definitly not including everything, and some of the choices are based on making a useful tool foundation.

## Don't Panic (Mr. Mannering ...)

The Arduino `tone()` function is not used. It never was (my mistake), as it's all done with the speaker. I must read more of the code again.
The esp32 board plugin must invalidate it in all likelyhood, preventing function redefinition errors. I don't think the **keyword highlight** helps
in this regard. 2026-01-27: You can tell I'm thinking about UI and multimedia today!

## Extra Notes (After Reading the Source)

- The uLisp documentation refers to "tail recursion" as a form. This contrary to the expectation of using less stack in a recursion loop,
just performs an  `eval` post execution. In this sense it does save a stack frame by continuing with the "selected" for return list. More
of a tail continuation, without an extra `eval` stack frame.

## Things I Might Do (Not Everything, but Alot with Simple Function Choices)

- [ ] Interface UI Builder - make apps easier to make using GFX toolkit, perhaps some PROGMEM graphics images.
For example 5 buttons on a row at 42*9 box, 6 tiny chars with 2 pix border padding. 2 pix button horizontal outer padding
and 5 pix horizontal gap extra (4 of these gaps).
- [X] Improve Edit Experience - `(edit 'func)` has arrows too (left, right and up tree) and `.` (cons prefix), `<backspace>` (delete), `(` (replace) and `<backtick>` (quit).
- [ ] Help Dialogs.
- [X] Scroll Lock - `ctrl` + `enter`(like last line given by `shift`+ `enter`). Use any key to continue scrolling another page if scroll is locked.
- [X] Keyboard - swap `()` and `[]` as lisp. Also `\` with `|` for arcane CS reasons and CTRL "causing" a SHIFT for `^\` (and `^[` on 9, not synthetic `ESC`).
- [ ] Maybe add some uses for `<backtick>`, `ctrl`, `opt`and `fn`.
- [X] Inverted Character Key - `Alt`for inverted characters.
- [ ] `directory` - the card is 8 GB. Is `cd`reasonable?
- [ ] Color Keywords - improving mental parsing, and real pretty printing.
- [X] Bignums - from negative integer, subtract larger for possible error. Division by zero.
- [ ] Bluetooth - maybe. Audio.
- [ ] Key Input -  maybe a non-blocking keyboard read.
- [X] Numbers - a few more number theoretic functions. For bignums and `(^ number*)` for floating point continued powers with a shorter name. Maybe `(nanp number)`.
- [X] List Reduce - a `(reduce 'op items*)` (right associative reduce) for any dyad operator with a `nil` parameter return of the reduction accumulator. As `(+)` does.
- [ ] Calculus - a few more calculus functions.
- [ ] Solver - some kind of variable solver.
- [ ] Streams - maybe there's more. Audio.
- [ ] Other Types - for rational and complex. Choice of natural R or P representation.
- [X] ULOS - Simple Object System.
- [X] Octo-Sound - uses the "pin" number as the channel number. As the Cardputer otherwise doesn't use it. Octave becomes duration. Add `(* 12 octave)` to the note. **Clicky**?
- [ ] Sound FX - the first seven channels are used (0 to 6), with channel 7 generating sound effects (if enabled), or just notes.   

## Other M5Carputer Projects

This list will expand as I place nice things I like.

* MiniAcid - ooh yes, tweeking for the ear.
* M5Launcher - a nice fine app as occasionally used!
* M5-Keyboard-Mouse - press that GO for keys or mouse. USB and Bluetooth.
* IR-Remote - apparently it does the lot.
* Wifi-Scan - clean show me the SSIDs.
* Porkchop - very entertaining.
* MicroHydra - yes, I'm using a Cardputer 1 for keys, but MicroPython.
