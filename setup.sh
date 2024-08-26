# This script is derived from Eisenberg's CS350 autograder
# 
# It installs GHC in the virtual box as well as any additional libraries.
# Everything is setup up via stack
#
apt-get update
apt-get -y install gcc make libnuma-dev libgmp10 libgmp-dev libtinfo-dev

echo "Getting ghcup"
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=9.6.6
export BOOTSTRAP_HASKELL_INSTALL_STACK=1

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# from https://github.com/haskell/ghcup/
#mkdir -p ~/.ghcup/bin
#curl https://raw.githubusercontent.com/haskell/ghcup/master/ghcup > ~/.ghcup/bin/ghcup
#chmod +x ~/.ghcup/bin/ghcup
echo "ghcup gotten"

echo "===>>Done installing ghc"

export PATH=$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH

# cabal update
# cabal -j1 v1-install alex
# cabal -j1 v1-install happy
# echo "===>>Done installing alex/happy"
stack install hlint
echo "===>>Done installing system packages"

cd /autograder
git clone https://github.com/sweirich/haskelltester.git
cd /autograder/haskelltester
stack install
echo "===>>Done installing gradescope utilities"

# Output installation summary
stack exec -- ghc-pkg list
stack path --local-bin  

ghc --version
cabal --version
stack --version
stack exec -- hlint --version
echo $PATH

echo "===>>All set up"
