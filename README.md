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
much about it. We are developing some of this right now to be able to
better establish which features stage1 needs to support.

Filetypes
---------
Currently the files used by stage0 and stage1 are given the extension
.yl this is because Make decided that .l files should be parsed by
lex, and they are not compliant .scm files yet. At some point this
will change, and we will probably rename them all, causing
confusion. Stage2 is currently written in scheme to do some
experiementation, and those files are .scm for this purpose.

Language Features
-----------------
TODO: Decide on features for each stage

Questions
---------
These are questions that either someone has already asked me, or that
we have had to answer for ourselves in starting this project.

Q: Why not use an existing lisp? Especially if you're using, for
example, R7RS, why not use Chibi Scheme?
PG: Well, apart from the fact I have a tendency to enjoy reinventing
wheels, there is the fact that every time I've looked at an existing
solution, there has been something that didn't quite work how I'd like
it to. As I have said, I'm not an experienced lisper, so the failure
is almost certainly on my part for not thinking in the lisp way. The
big picture reason for this is to see if I can get a lisp which ticks
all the boxes that I think I want, or, more likely, to figure out why
I don't actually want them.

Q: Ok, so what features do you want in this?
PG: How long do you have? I mean, among the things I want is to have a
natively compiled lisp, I want hot reloading of code, I want an
interpreted lisp, ideally with seemless native-interpreted interop, I
want threading, I want GPGPU support, I want to be able to write
shaders and graphics applications entirely in lisp, I want to be able
to create DSLs to solve my problems... as you can see the list goes on
and on.

Q: But X already does all of that.
PG: Ok. Noted. I'll look at that for inspiration when it comes to
it. I still want to learn how to do it myself.

Q: That will probably take you a lifetime to develop
PG: I'm not expecting to have the perfect lisp to give me all of these
things. I am using this as a testbed for learning. If I succeed in
ticking some of those boxes, I'll be happy. 

Q: Can I contribute?
(no-one actually asked this... but I can hope)
PG: Sure. The more the merrier!

Contributors
------------
Nistur - Philipp Geyer
BitPuffin - Isak Andersson