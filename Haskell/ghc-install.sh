#! usr/bin/env bash

DNF="sudo dnf install"
$DNF' ghc cabal-install';
$DNF' haskell platform';
$DNF' ghc-*devel';
$DNF' cabal-rpm';

