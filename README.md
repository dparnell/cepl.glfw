# Cepl.GLFW

This package contains code that allows [GLFW](http://www.glfw.org/) to be used as a host for [CEPL](http://techsnuffle.com/cepl/api.html).
It is very young but does appear to render stuff.

## Installation

Make sure that the GLFW shared library is installed. See [here](http://www.glfw.org/download.html) to download a pre-built copy of GLFW for Windows.

## Usage

```lisp
(ql:quickload "cepl.glfw")
(load "example/triangle.lisp")
(cepl.glfw.triangle:run-demo)
```
