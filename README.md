qmake-ng
======

**qmake-ng** is a Qt Toolkit qmake replacement, implemented in the D programming language using [Pegged](https://github.com/PhilippeSigaud/Pegged).
Pegged is a parsing expression grammar ([PEG](http://en.wikipedia.org/wiki/Parsing_expression_grammar)) generator.
To resolve qmake grammar ambiguity, custom preprocessor was written and used.

Status
--------
Currently only parser was implemented - all Qt Toolkit project files are successfully processing.
So, qmake-ng can be useful as strict linter for project files.

Usage
--------
```
# Parse project
qmake-ng myproject.pro

# Recursively parse all projects in directory
qmake-ng ~/my/dir
```

Goals
--------
* qmake parser silently skip some syntax errors (i will create a corresponding QTBUG when i will have time)
* qmake parser behaviour is not obvious in complex scenario (e.g. whitespace between function name and parenthesis)
* qmake can only generate makefiles/etc, but cannot interpret project on-the-fly like qbs
* qmake source code heavily use Qt private infrastructure and  cannot be easily used in your own project

There is no official qmake project grammar, so i was forced to restore it using valid project examples.
Something like Rosetta stone :D
So, the current version of grammar cannot be proved to be 100% valid.
However, it already parsed all of Qt's itself project files (~5000).

Pegged usage allowed to make parser much more strict than the original one.
E.g. original qmake just ignores extra closing parenthesis, but qmake-ng failes:
```
message(Hello))
```

Plans
--------
* qmake project files parser/linter with AST generation (DONE)
* semantic analyzer using AST (TODO)
* built-in test and replace functions implementation
* qbs-like on-the-fly interpretation of project commands without makefiles generation (TODO)

