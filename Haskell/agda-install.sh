#! usr/bin/env bash
# How to automate a sha-bang

cabal update;
cabal intall agda;
echo 'export PATH="$HOME/.cabal/bin:$PATH"' >> ~/.bashrc';
source ~/.bashrc

exit $#
