            = The Lovely Haskell Environment =

A talk that is also a desktop-cleaning Haskell program and a
justification for literate programming languages!

> module DesktopCleaner -- see! fucking awesome



                      = Productivity =

Scripting languages like Ruby and Python are often praised
for their users ability to be productive in them.

[productivity]
  being able to generate, create, enhance, or bring forth
  goods and services



                   = A Standard to Meet =

What makes Ruby productive?

  * malleability - reopen classes, overwrite methods, etc
  * introspection - query a program about itself
  * gems - reusable chunks of code



                    = More Productive =

Haskell, through tools like __GHCi__ and __GHCmod__,
provides means for greater productivity:

  * less malleability -> infer more about code
  * on-the-fly compiler feedback > introspection
  * Cabal :( (but we can make it okay)



                   = Let's get started! =

We need two tools from (preferably) our package manager:

  * GHC
  * cabal-install

@
  $ brew install ghc cabal-install
@



                     = Updating Cabal =

We can minimize __cabal hell__ by installing only the
minimum required global libraries.

  * cabal-install
  * happy
  * ghc-mod

@
  $ cabal install cabal-install happy ghc-mod
@



                      = Intermission =

Zzzz...



= 
