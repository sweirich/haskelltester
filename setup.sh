# This script is derived from Eisenberg's CS350 hw 03 autograder
# 
# It installs GHC in the virtual box as well as any additional libraries.
# Everything is setup up via cabal v1 global installation
#
apt-get update
apt-get -y install gcc make libnuma-dev libgmp10 libgmp-dev 

echo "Getting ghcup"
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=9.0.2
export BOOTSTRAP_HASKELL_INSTALL_STACK=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# from https://github.com/haskell/ghcup/
#mkdir -p ~/.ghcup/bin
#curl https://raw.githubusercontent.com/haskell/ghcup/master/ghcup > ~/.ghcup/bin/ghcup
#chmod +x ~/.ghcup/bin/ghcup
echo "ghcup gotten"

export PATH=$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH


echo "===>>Done installing ghc"

# cabal update
# cabal -j1 v1-install alex
# cabal -j1 v1-install happy
# echo "===>>Done installing alex/happy"
# cabal -j1 v1-install HUnit QuickCheck hlint

# cabal -j1 v1-install hlint
# -j1 there is needed to that Gradescope doesn't kill us for being a hog
stack install hlint
# stack install HUnit-1.6.2.0
# stack install QuickCheck-2.14.2
# stack install containers-0.6.4.1
# stack install aeson-2.0.3.0
# stack install bytestring-0.10.12.1
# stack install text-1.2.5.0
echo "===>>Done installing system packages"

cd /autograder
git clone https://github.com/sweirich/haskelltester.git
cd /autograder/haskelltester
stack install
echo "===>>Done installing gradescope utilities"

# Output version information
# ghc --version
# cabal --version
# 

ghc --version
cabal --version
stack --version
stack exec -- hlint --version
echo "===>stack path"
stack path --local-bin  
echo $PATH
echo "===>>All set up"
