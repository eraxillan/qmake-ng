qmake-ng
======

**qmake-ng** is a desperate attempt to write Qt qmake replacement.
Development stack: D programming language + DUB + [Pegged](https://github.com/PhilippeSigaud/Pegged).
Pegged is a parsing expression grammar ([PEG](http://en.wikipedia.org/wiki/Parsing_expression_grammar)) generator.
To resolve qmake grammar ambiguity, custom preprocessor was written and used.

Status
--------
Currently only lexer and parser were implemented.
All Qt source project files are successfully parsed.
So, qmake-ng can be useful at least as a strict linter for qmake project files.

Usage
--------
```
# Parse project
qmake-ng myproject.pro

# Recursively parse all qmake projects in directory
qmake-ng ~/my/dir
```

Goals
--------
* qmake parser silently skip some syntax errors ([e.g.](https://bugreports.qt.io/browse/QTBUG-65785))
* qmake parser behavior is not obvious in complex scenario (e.g. white space between function name and parenthesis)
* qmake can only generate makefiles/etc, but cannot interpret project on-the-fly like qbs
* qmake source code heavily use Qt private infrastructure and cannot be easily fixed by stranger

Pegged usage allowed to make parser much more strict than the original one.
E.g. original `qmake` just ignores extra closing parenthesis, but `qmake-ng` fails:
```
message(Hello))
```
## Grammar ambiguity

There is no official qmake project grammar.
So, i was forced to "restore" it using valid project examples.
Something like Rosetta stone :D
Moreover, `qmake` grammar is just ambiguous:
```
windows:x64:error("x86_64 still not supported")
```
Here the first colon means logical `AND`, but the last one separates scope condition from true-branch.
Another example:
```
contains(CONFIG, ^(android)$)
```
Notice that second function argument is not enquoted.
Parser must somehow detect whether argument is a regular expression and process it separately.

So one should understand - the current version of grammar cannot be proved to be complete.
However, it already parsed all of Qt's itself project files (~5000).
And i hope it cover the most of qmake syntax stuff.

Plans
--------
* qmake project files parser/linter with AST generation (DONE)
* semantic analyzer using AST (TODO)
* built-in test and replace functions implementation
* qbs-like on-the-fly interpretation of project commands without makefiles generation (TODO)

