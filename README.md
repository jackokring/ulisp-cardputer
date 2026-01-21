# Cardputer uLisp Machine
A version of uLisp to convert the M5Stack Cardputer into a self-contained handheld Lisp computer. The files are as follows:

* ulisp-cardputer.ino - the uLisp source file, to be compiled and uploaded using the Arduino IDE.
* Bignums.ino - big numbers as a types.

About 5% of memory is available after build. To get 37% free use "Minimal SPIFFS" partition scheme for M5Launcher
compatibility.
