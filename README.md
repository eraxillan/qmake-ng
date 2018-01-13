qmake-ng
======

**qmake-ng** is a Qt Toolkit qmake replacement, implemented in the D programming language using [Pegged](https://github.com/PhilippeSigaud/Pegged).
Pegged is a parsing expression grammar ([PEG](http://en.wikipedia.org/wiki/Parsing_expression_grammar)) generator.
To resolve qmake grammar ambiguity, custom preprocessor was written and used.

Status
--------
Currenly only parser was implemented - all Qt itself project files are successfully processing.
Can be useful as linter.

Usage
--------
```
qmake-ng myproject.pro
```

Why?
--------
* qmake parser have bad quality, e.g. skip some syntax errors (check BUG- FIXME:)
* qmake can only generate makefiles/etc, but can't interpret project on-the-fly like qbs
* qmake code heavily use Qt private infrastructureand  cannot be easily used in your own project
There is no official qmake project grammar, so i was forced to restore it using valid project examples.
So current version of grammar cannot be proved to be 100% valid. However, it parsed all of Qt's project files (~5000).

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

