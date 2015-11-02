#!/bin/sh

SNIPPET_DIR="snippets/$1"
OUT_DIR="$SNIPPET_DIR/dist"

BUILD_IT="ghcjs --make -j4 -static \
-outputdir $OUT_DIR \
-odir $OUT_DIR \
-hidir $OUT_DIR \
-stubdir $OUT_DIR \
-isnippets \
-I$SNIPPET_DIR \
-XHaskell2010 \
$SNIPPET_DIR/Main.hs \
-O2 \
-Wall \
-fno-warn-unused-imports \
-fno-warn-unused-do-bind \
-fno-warn-orphans"

if ! command -v nix-shell >/dev/null ; then
  echo Setting up environment
  . ~/.nix-profile/etc/profile.d/nix.sh
fi

nix-shell -A env --pure -j 8 -I ../deps --command "$BUILD_IT"
#cat $SNIPPET_DIR/Main.jsexe/out.js $SNIPPET_DIR/Main.jsexe/runmain.js > $SNIPPET_DIR/out.js
cp $SNIPPET_DIR/Main.jsexe/all.js $SNIPPET_DIR/out.js
gzip -k $SNIPPET_DIR/out.js
#rm -fr $SNIPPET_DIR/Main.jsexe $SNIPPET_DIR/dist
cp template.html $SNIPPET_DIR/index.html
echo "<script language='javascript' src='/$SNIPPET_DIR/out.js' defer></script></html>" >> $SNIPPET_DIR/index.html
