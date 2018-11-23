Milestone 2:  Semantic Analysis to Intermediate Representations

Read chapters
   5 (Semantic Analysis),
   6 (Activation Records), and
   7 (Translation to Intermediate Trees),
in the textbook, and complete the program sections from each chapter.

You may work from your Milestone 1 files, or you may use the supplied ones in Milestone 2 starter.  They include

      absyn.sml    -- Absyn structure: the abstract syntax

      errormsg.sml -- ERRORMSG signature and ErrorMsg structure

      main.sml     -- Main structure that with a main that parses
                      Tiger source files to abstract syntax and either
                         -p => prints in SML constructors
                         -u => unparses into concrete syntax

                         -e => turns on escape analysis
                         -c => turns on type checking and translation

                         -t => turns on trace scheduling

			 -x => turns on an interpreter back end

As the starter for Milestone 2, I will also supply additional files:

       temp.sig -- the TEMP signature
       temp.sml -- the Temp structure

       types.sml -- the Types structure

       tree.sml -- the TREE signature and partial Tree structure

       printtree.sml -- functions to display IR trees
       unparsetree.sml -- functions to print IR trees

       frame.sig -- the FRAME signature

       interp.sml -- the interpreter

The files you must submit are:
    semant.sml -- the Semant structure, including support for
                     * mutually recursive functions
                     * mutually recursive types
                     * checks for break outside while/for
                  and appropriate calls to Translate.

    env.sml -- the ENV signature and Env structure extended
                 to support nesting levels and static links
                 and populated with initial entries

    translate.sml -- the TRANSLATE signature and Translate structure
                     including the Ex, Nx, and Cx constructors

    riscvframe.sml -- a Frame structure for the RV32I architecture
                     handling k=8 formals passed in registers, k=2
                     return values in registers, and the rest on the
                     stack.  The ABI is documented in chapter 20 and
                     the instructions in Chapter 2 of the User-Level
                     ISA specification at
                     https://riscv.org/specifications/. We will use
                     the sp and fp registers, and at your discretion,
                     the gp register.  I will post the summary
                     (chapters 2--3) of "The RISC-V Reader" because
                     much of the specification details are irrelevant:
                     we are not interested in the supervisor-level
                     instructions (control registers etc...) and we're
                     targeting assembly language, so we don't care
                     about the bits-level encoding of the
                     assmebly-language instructions or their
                     compression.

    escape.sml -- the FindEscape structure with appropriate
                     modifications to transDec

As always, do the simple things first. and built up.  Start with
constant expressions, then larger expressions; do single declarations
then proceed on to multiple-declaration blocks, and last recursive
declarations.

Due: November March 18, 2018.


