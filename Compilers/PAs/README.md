#Cool Compiler

# PA2J: Lexical Analyzer

This directory contains the classes and flex file used by JFlex to create
a lexer.

-cool.lex: JFlex input file
-CoolLexer.java: File that contains generated flex file, along with some
 extra boilerplate for incorporating lexer with other created classes.
-pa1-grading.perl: Automated script for grading accuracy of lexer

input "make lexer" to generate the lexer files.

# PA3J: Syntax Analyzer/Parser Generator

This directory contains all the files for Cup, a java-based parser generator.

-cool.cup: Grammar Specification for Cup parser generator
 The result of running cool is a number of classes containing the various
 features of the language.

Run "make parser" to generate the parser files.

#PA4J: Semantic Analyzer

This directory contains a collection of all classes produced by the parser,
and some utility classes used to perform semantic analysis upon target *.cl
files.

-cool-tree.java: Collection of all Non-Terminal elements for the language
-ClassTable.java: A utility data structure for storing classes and analyzing
 inheritance heirarchies
-TreeConstants.java: A collection of AbstractSymbol classes used for type
 analysis between Non-Terminal classes
