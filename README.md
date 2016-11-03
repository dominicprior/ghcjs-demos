# ghcjs-demos

This project is for learning enough ghcjs to write graphics programs for the browser.

To get ghcjs working, I used stack in an Ubuntu 16 VM, and followed the instructions here: https://docs.haskellstack.org/en/stable/ghcjs/.  I followed the instructions very carefully, because a previous attempt (on an earlier Ubuntu) failed, possibly due to not having installed all the pre-requisites that are mentioned here: https://github.com/ghcjs/ghcjs

## hello4

The starting point for my first working example, hello4, was this: https://github.com/ghcjs/ghcjs-dom-hello.

To build hello4, I just run "stack build", and, to try it out, I view inside chromium the index.html from the hello.jsexe folder that "stack build" mentions.

The program responds to clicks in the window by drawing the text at that position.
