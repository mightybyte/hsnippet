# HSnippet

## Developer Notes

* Resources are designed such that all static CSS and JS should go in the
  appropriate subdirectory in `$PROJECT_ROOT/static`.  A Makefile is set up to
  concatenate all these assets and place them in app/static/app.css and
  app/static/app.js respectively.  The only exception to this is the
  GHCJS-generated javascript, which is kept separate so not every page has to
  incur the large download cost if it doesn't use GHCJS-generated javascript.
  This allows all HTML templates to have a simple set of CSS and JS includes.
