Milestone 1 -- Compiler Front-End
Due date: January 29, 2016
=================================


As a group, complete the PROGRAM for Chapter 4 of the textbook.  Please note that

       A) the Tiger language is documented in Appendix A; but I have
          enriched it with actual booleans.  As a result,
                1) there is another built-in type, named `bool'

                2) there is another expression in absyn.sml:
                             exp = ...
                               | BoolExp of bool
                   with concrete syntax `true' and `false'

                3) the `if' and 'while' statements no longer rely on
                   zero or non-zero to distinguish the cases

                4) the various equality and inequality operators
                   (=,<>,>,<,>=,<=) produce boolean results.

                5) the translations of and (`&') and or (`|') change:

                       e1 & e2   <==> if e1 then e2 else false
                       e1 | e2   <==> if e1 then true else e2

                6) the standard library function `not' inverts boolean
                   values, not integers.
               
          This change will cascade through the compiler, especially
          the type-checker.

       B) You are supplied with a working (but incomplete) system;
          please add to tiger.lex and tiger.gram. There are two
          middle-ends supplied:

                      -a will print the abstract syntax in
                                        constructor form

                      -u will print the program out in (unparsed)
                                        concrete syntax form

          The Makefile may require customization of paths to
          accommodate your system configuration.  The default

                  make

          target should build the compiler.

       C) Tests and a rudimentary testing harness are included with the
          starter files.  Please try the

                  make test

          target to see what happens.

       D) Note that there are many edge cases in the syntax: see Appendix A
          in the textbook for complete details.

                1) there are many escape sequences allowed in strings

                2) a sequence expression must contain at least two
                   sub-expressions

                3) integer arithmetic has the usual precedence and
                   associativity

                4) assignment is lower precedence than boolean
                   operators

Your parser should have exactly zero conflicts of any kind.  Reduce-reduce
and shift-shift conflicts are deadly.  Getting rid of the last two shift-reduce
conflicts will require some cleverness.

Please tag your final milestone-1 compiler with the "M1" tag in the gitlab
repository.  I will collect it from there for marking.

II. Individual Work
-------------------

Be prepared to answer exam questions like
	(a) write an NFSA for the some regexps
	(b) construct the DFSA for it
	(c) compute the first and follow sets for the some grammar 
	(d) given a grammar and an input, trace its execution and report where it fails
	(e) what were the last two conflicts and how did you eliminate them

