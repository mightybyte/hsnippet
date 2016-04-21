# HSnippet

## TODO

See the issue tracker at https://github.com/mightybyte/hsnippet/issues

## Developer Notes

Resources are designed such that all static CSS and JS should go in the
appropriate subdirectory in `$PROJECT_ROOT/static`.  A Makefile is set up to
concatenate all these assets and place them in `app/static/app.css` and
`app/static/app.js` respectively.  The only exception to this is the
GHCJS-generated javascript, which is kept separate so not every page has to
incur the large download cost if it doesn't use GHCJS-generated javascript.
This allows all HTML templates to have a simple set of CSS and JS includes.

## Directory Structure

* /app          - The directory from which you run the backend server
* /backend      - Cabal project for the backend web server
* /deps         - Git submodules for dependencies not available on hackage
* /frontend     - Frontend Haskell code that gets compiled to js with build.sh
* /hsnippet-lib - Small library of functionality provided to snippets
* /sandbox      - Sandbox where snippets are built
* /shared       - Code needed by both the backend and frontend
* /static       - Static resources

The above mentioned Makefile concatenates CSS and JS in /static and puts the
output in app so pages don't need to depend on a bunch of separate files.

## Building for the first time

    # Update everything from git
    git pull
    git submodule update --init --recursive

    # Set up the cabal sandbox
    ./init-sandbox.sh

    # Build the backend
    cd backend && cabal isntall --only-dependencies --force-reinstalls && cabal build
    cd ..

    # Build the frontend
    cd frontend && ./build.sh     # Must have previously installed nix with try-reflex
    cd ..

    # Set up all the static resources
    make

    cd app
    cp devel.cfg.template devel.cfg

    # Manual action required...edit devel.cfg to point to a local postgres server

    ../backend/dist/build/main/main
