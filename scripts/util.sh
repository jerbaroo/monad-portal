# User account for Cachix integration.
CACHIX_USER='jerbaroo'

# GHC or GHCJS packages parsed from default.nix.
# > echo $(packages 'ghc')
# telescope telescope-ds-file etc.
packages () {
  echo $(sed "/## BEGIN-$1 ##/,/## END-$1 ##/!d" default.nix | sed '/homepage/d' | sed  '/##/d' | sed 's/"//g')
}

# echo $(sed "/## BEGIN-GHC ##/,/## END-GHC ##/!d" default.nix)
# Two arrays of packages for compilation with GHC or GHCJS respectively.
# > echo ${GHC_PKGS[*]}
# telescope telescope-ds-file etc.
GHC_PKGS=($(packages 'GHC'))
GHCJS_PKGS=($(packages 'GHCJS'))

# Two strings of Nix attributes, for compilation with GHC or GHCJS respectively.
# > echo $NIX_GHC_ATTRS
# -A telescope -A telescope-ds-file etc.
NIX_GHC_ATTRS=''; NIX_GHCJS_ATTRS='';
for PKG in "${GHC_PKGS[@]}";   do NIX_GHC_ATTRS="$NIX_GHC_ATTRS -A ghc.$PKG";       done
for PKG in "${GHCJS_PKGS[@]}"; do NIX_GHCJS_ATTRS="$NIX_GHCJS_ATTRS -A ghcjs.$PKG"; done

# Flags that should be passed to cabal.
# TODO: ensure the flags are also used by Nix.
flags () {
  echo " \
--ghc-options=-Wall \
--ghc-options=-threaded \
--test-show-details=streaming"
}
