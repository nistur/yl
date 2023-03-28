YL
==
Yoctolisp
=========

This is an attempt at creating a lisp. There are a few reasons why I
am writing one from scratch rather than using an existing one. The
primary reason is masochism. Secondary is that I haven't yet found a
use case where I could justify using lisp over something I was more
familiar with. The third reason is for what I would like to make, I
have always had minor issues with existing lisps. Whether these were
things in the language which were missing or, more likely, bits
missing from my understanding is largely irrelevant. This project will
be able to fill both gaps if they exist.

Goal
----
The goal for yoctolisp (`yl`) is to create a compiled lisp which I can
use for a few projects. There are multiple that I have in mind, which
range from some small command line tools that I could probably knock
together in a couple of lines of Bash more efficiently, through GUI
applications, to video games. I don't necessarily expect to be able to
do all of these things, but it does help define that I am attempting
to create a general purpose language.

I want the dependencies for the language to be minimal. Ideally I
would like yl to be entirely self hosting, meaning that the yl
compiler is written in yl. This is obviously not possible without an
existing yl compiler, so there is a bootstrap stage0 which is written
in C. I did debate having a yl compiler written in another lisp, but
there are two things speaking against this, first I don't really feel
comfortable enough with lisp to dive in and write a compiler - I was
more comfortable learning the innards of writing an interpreter, but
also, as I said, I want the dependencies to be minimal, and from
experience, depending on gcc is much easier than depending on SBCL, or
chibi-scheme, or something. I may regret the decision, but for now I
believe it's the right one.

For the most part, the design will be implementing Scheme R7RS
small. At the moment no part of it is in any way compliant at all,
this will hopefully change in the near future, but for the time being,
I'm bootstrapping both the compiler, and my understanding.

GOAL
----
One of the key inspirations for this is Naughty Dog's GOAL, which they
used for creating games such as Jak and Daxter.

Stage0
------
This is a small C interpreter with limited features. Its only use case
is to run stage1. It currently does appear to leak some memory, and
also I am not convinced that it is definitely releasing at the right
time, but it appears to be working right now, and as its only purpose
is to run stage1, it's not the highest priority right now.

Stage1
------
This is the current stage of development. This is a compiler written
in lisp. This will run initially on stage0, and will compile
itself. Its next task is to use the compiled stage1, to recompile
itself. It can then compare the output to ensure that stage1 is
reproducible. This will be a superset of functionality in stage0, but
a subset of stage2. The features should be kept relatively minimal as
the initial time it is run, it will be run through an unoptimised
interpreter

Stage2
------
This will be the final compiler. It will be compiled with stage1. This
will include all of the language features, a whole standard library
etc. At this point this is too far in the future to be able to put
much about it.

Language Features
-----------------
TODO: Decide on features for each stage