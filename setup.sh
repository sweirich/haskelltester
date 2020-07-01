# This script is derived from Eisenberg's CS350 hw 03 autograder
# 
# It installs GHC in the virtual box as well as any additional libraries.
# Everything is setup up via cabal v1 global installation
#
apt-get update
apt-get -y install gcc make libnuma-dev libgmp10 libgmp-dev

# from https://github.com/haskell/ghcup/
echo "Getting ghcup"
mkdir -p ~/.ghcup/bin
curl https://raw.githubusercontent.com/haskell/ghcup/master/ghcup > ~/.ghcup/bin/ghcup
chmod +x ~/.ghcup/bin/ghcup
echo "ghcup gotten"

export PATH=$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH

echo "===>>Installing ghc"
ghcup install
ghcup set
ghcup install-cabal
echo "===>>Done installing ghc"
ghc --version
cabal --version

cabal update
cabal -j1 v1-install alex
cabal -j1 v1-install happy
echo "===>>Done installing alex/happy"
cabal -j1 v1-install HUnit QuickCheck hlint

cabal -j1 v1-install hlint
# -j1 there is needed to that Gradescope doesn't kill us for being a hog

echo "===>>Done installing system packages"

cd /autograder/source/gradescope
cabal -j1 v1-install

echo "===>>Done installing gradescope utilities"

# Output version information
ghc --version
cabal --version
ghc-pkg list

echo "===>>All set up"
