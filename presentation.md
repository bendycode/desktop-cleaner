# The Lovely Haskell Environment

Make Haskell the most productive language in your toolset!



# Productivity

Scripting languages like Ruby and Python are often praised
for their users ability to be productive in them.

- _Definition_: being able to generate, create, enhance, or
  bring forth goods and services.



# A Standard to Meet

What makes Ruby productive?

- _malleability_: reopen classes, overwrite methods, etc.
- _introspection_: query a program about itself
- _gems_: reusable chunks of code



# More Productive

Haskell, through tools like _GHCi_ and _GHCmod_,
provides means for greater productivity:

- well-structured > malleable for inferring about program
- on-the-fly compiler feedback > introspection
- Cabal :( (but we can make it okay)



# Let's get started!

We need two tools from (preferably) our package manager:

- GHC
- cabal-install

```sh
brew install ghc cabal-install
```

_IMPORTANT_ Add `~/.cabal/bin` to your `$PATH`



# Updating Cabal

We can minimize _cabal hell_ by installing only the
minimum required global libraries.

- _cabal-install_: our kinda/sorta omni-tool
- _happy_: just a dependency of ghc-mod
- _ghc-mod_: our productivity workhorse
- _hspec_: lovely spec library

```sh
cabal install cabal-install-1.20.0.6
cabal install happy ghc-mod hspec
```



# Intermission

Zzzz...



# Setting Up a Project Template

To get up and running quickly, we want to set up a project
template. This will accomplish the following:

1. supply the REPL with our project files
2. allow project-specific library installation (ala gemsets)
3. enable easy testing



# Setting Up a Project Template (cont...)

- We'll run through a series of shell commands
- Also may clone or reference [an small example project][1]

  [1]: https://github.com/Jonplussed/desktop-cleaner



# 1. Getting Organized

In a new project directory, add the following:

- `src/` for our modules
- `test/` for tests, mirrioring `src/`



# 2. Preparing Our Toolset

So that Cabal knows of our project, in our project dir, do:

1. `$ cabal init` to create a cabal file
2. `$ cabal sandbox init` to keep libraries local
3. `$ cabal configure --enable-tests` so we can test things
4. see `desktop-cleaner.cabal` for necessary structure (all
   fields shown are required)



# 2. Preparing Our Toolset (cont..)

Cabal file fields of note:

- `executable [project name]`
- `test-suite [arbitrary name]`
- `main-is`
- `hs-source-dirs`



# 3. Making Testing Easy (the hard way)

See [Hspec's automatic test discovery][2] for more
information.

  [2]: http://hspec.github.io/hspec-discover.html

...but it boils down to:

- create the file `test/Spec.hs`
- include in it only a single line:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```



# Let's See What This Gets Us

We're going to reference [desktop-cleaner][1] again for a
working example. When cloning a project, you must still:

- `cabal sandbox init`
- `cabal install --enable-tests --only-dependencies`
- `cabal configure --enable-tests`



# Useful Cabal Commands

- `cabal repl` to load GHCi with our project files
- `cabal test` to run our specified test-suite
- `cabal build` to create our executable
- `cabal run` to build our exec and immediately run it
- `runhaskell` to run your `.hs` files as a script



# But Wait, There's More!

_GHC-mod_ is your text-editor's best friend. We installed it
earlier; now let's hook it into Vim.

We'll need [ghcmod-vim][3] and [neco-ghc][4], which can be
installed via Pathogen or Vundle.

  [3]: https://github.com/eagletmt/ghcmod-vim
  [4]: https://github.com/eagletmt/neco-ghcE



# A Few Bindings Away from HASKELL ZEN

Some keybindings for easily exploring your project files
using [ghcmod-vim][3]:

```vim
nnoremap <buffer> <Leader>ht :GhcModType<CR>
nnoremap <buffer> <Leader>hh :GhcModTypeClear<CR>
nnoremap <buffer> <Leader>hc :GhcModCheck<CR>
nnoremap <buffer> <Leader>hl :GhcModLint<CR>
```

# HOLY SWEET CRAP: (mostly) SCOPE-AWARE AUTOCOMPLETE

Set Vim's `omnicomplete` functionality via [neco-ghc][4],
accessible via `C-X C-O`

```vim
au FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:necoghc_enable_detailed_browse = 1
```



# Finally, Prettier Testing

`cabal test` has a few options for nice-looking tests, which
we can wrap in a Vim shortcut:

```vim
au FileType haskell nnoremap <buffer> <Leader>t :! cabal \
  test --show-details=always --test-options="--color"<CR>
```



# Congratulations!

You now hold the powers of creation that once only God could
command. Use this power wisely!
