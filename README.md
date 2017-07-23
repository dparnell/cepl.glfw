# Cepl.GLFW

![Screenshot](/images/screenshot.png/)

This package contains code that allows [GLFW](http://www.glfw.org/) to be used as a host for [CEPL](http://techsnuffle.com/cepl/api.html).

## It works on my machine

![It works on my machine](/images/certification.png)
Cepl.GLFW is very young but does appear to render stuff. I have only run it on Windows 10 under SBCL, but who knows, it might work on other setups.

## Requirements

1. Make sure that the GLFW shared library is installed. See [here](http://www.glfw.org/download.html) to download a pre-built copy of GLFW for Windows.
2. You will also need [libffi](http://sourceware.org/libffi/) installed. The easiest way to get this on Windows is to use the one that ships with [Emacs](https://www.gnu.org/software/emacs/).

## Usage

```lisp
(ql:quickload "cepl.glfw")
(load "example/triangle.lisp")
(cepl.glfw.triangle:run-demo)
```
