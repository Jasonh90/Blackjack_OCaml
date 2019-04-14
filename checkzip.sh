#!/bin/bash
# DO NOT EDIT THIS FILE

ZIPFILE=game.zip
TESTFILE=dictionary.ml

zip=good

if [ -e $ZIPFILE ]; then
  echo "$ZIPFILE exists.  Good."
  if zipinfo -1 $ZIPFILE | grep -q "^$TESTFILE$"; then
    echo "$TESTFILE is in the root folder of the zip.  Good."
  else 
    echo "$TESTFILE is NOT in the root folder of the zip.  This is bad."
    echo "Did you create the zip by running \"make zip\"?"
  zip=bad
fi
else
  echo "$ZIPFILE does NOT exist.  This is bad.  Have you run \"make zip\"?"
  zip=bad
fi

if [[ "$zip" == good ]]; then
  cat <<EOF
===========================================================
Your zip file looks good to me.  Congratulations!
===========================================================

I checked to make sure your zip contains at least one of
your source files, but I can't check to make sure it contains
all of them, because I can't know what modules or test files
you might have added.  As long as you created the zip by running 
"make zip", it will have them.  But just in case you want to 
double check, run this command:  
  $ zipinfo -1 $ZIPFILE
EOF
else
  cat <<EOF
===========================================================
WARNING

Your zip file looks broken to me.  The code that
you submit might not compile on the grader's machine,
leading to heavy penalties.  Please fix your zip file.
Check the error messages above carefully to determine 
what is wrong with your file.  The most common mistake
is creating the zip using your OS's file browser, instead
of using "make zip".  See a consultant for help if you 
cannot determine what is wrong.
===========================================================
EOF
fi