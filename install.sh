link() {
  from="$1"
  to="$2"
  echo "Linking '$from' to '$to'"
  rm -f $to
  ln -s "$from" "$to"
}

if [[ -d nixpkgs ]]; then
  cd nixpkgs
  git fetch
  git remote update channels
  git checkout master
  git reset --hard origin/master
  #git checkout 15.09
  #git rebase channels/nixos-15.09
  cd ..
else
  git clone https://github.com/NixOS/nixpkgs.git nixpkgs
  cd nixpkgs
  git remote add channels https://github.com/NixOS/nixpkgs-channels.git
fi

if [[ -d vim/bundle/vundle ]]; then
  cd vim/bundle/vundle
  git pull origin master
  cd ../../../
else
  git clone https://github.com/VundleVim/Vundle.vim.git vim/bundle/vundle
fi

if [[ ! -d ~/.nix-defexpr ]]; then
  mkdir -p ~/.nix-defexpr
  link "$HOME/.dotfiles/nixpkgs" "$HOME/.nix-defexpr/nixpkgs"
fi

for location in $(find $HOME/.dotfiles -maxdepth 1 -name '*' ! -path '*.git'| sort); do
  name=$(basename $location)
  link "$HOME/.dotfiles/$name" "$HOME/.$name"
done
