#! usr/bin/env bash

git clone "https://github.com/agda/agda-stdlib.git";
cd agda-stdlib;
mkdir -p ~/.agda;
echo "$PWD/standard-library.agda-lib" >> ~/.agda/libraries;
echo "standard-library" >> ~/.agda/defaults

exit
