# Mira.cl is a modular bot framework in Common Lisp

The project is an old common lisp project of mine that implements
a framework to build chat bots, most notably irc bots.

It provides two basic abstractions:

1. channels - these are input and output channels that the bot can use for communication
2. processors - those are custom handlers that deal with inputs from channels

### Up and running

The code has only been tested with SBCL and requires [quicklisp](https://www.quicklisp.org/beta/#installation).
There is no documentation other than the code and since the project is quite dated I will probably also not 
add it. Think of it as a library that provides the building blocks for bot. If your can work with common lisp
the code should be fairly self explenatory though.

Enjoy digging in :)
