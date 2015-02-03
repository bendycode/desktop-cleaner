# The Lovely Haskell Environment

Make Haskell the most productive language in your toolset



# Productivity

Scripting languages like Ruby and Python are often praised
for their users ability to be productive in them.

- _Definition:_ being able to generate, create, enhance, or
  bring forth goods and services.



# A Standard to Meet

What makes Ruby productive?

- _malleability:_ reopen classes, overwrite methods, etc.
- _introspection:_ query a program about itself
- _gems:_ reusable chunks of code



# More Productive

Haskell, through tools like _GHCi_ and _GHCmod_,
provides means for greater productivity:

- less malleability -> infer more about code
- on-the-fly compiler feedback > introspection
- Cabal :( (but we can make it okay)



# Let's get started!

We need two tools from (preferably) our package manager:

- GHC
- cabal-install

    $ brew install ghc cabal-install

_IMPORTANT:_ Add `~/.cabal/bin` to your `$PATH`



# Updating Cabal

We can minimize __cabal hell__ by installing only the
minimum required global libraries.

- cabal-install
- happy (dependency of ghc-mod)
- ghc-mod
- hspec

    $ cabal install cabal-install-1.20.0.6
    $ cabal install happy ghc-mod hspec



# Intermission

Zzzz...



# Setting Up a Project Template

To get up and running quickly, we want to set up a project
template. This will accomplish the following:

1. supply the REPL with our project files
2. allow project-specific library installation (ala gemsets)
3. enable easy testing

We can do this in just a few shell commands:



# Setting Up a Project Template

1. setting up a new project structure

    $ mkdir my_project && cd my_project
    $ mkdir src test
    $ touch src/Main.hs test/MainSpec.hs



# Setting Up a Project Template

2. initializing Cabal

    $ cabal init
    $ cabal sandbox init



# Make Testing Easy



    $ echo '{-# OPTIONS_GHC -F -pgmF hspec-discover #-}' \
    $ test/Spec.hs



# Okay-- That Was Kinda Gross


