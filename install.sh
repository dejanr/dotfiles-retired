if [[ -d nixpkgs ]]; then
  cd nixpkgs
  git fetch
  git remote update channels
  git checkout 15.09
  git rebase channels/nixos-15.09
  cd ..
else
  git clone https://github.com/NixOS/nixpkgs.git nixpkgs
  cd nixpkgs
  git remote add channels https://github.com/NixOS/nixpkgs-channels.git
fi

if [[ -d nixpkgs ]]; then
  cd vim/bundle/vundle
  git pull origin master
  cd ../../../
else
  git clone https://github.com/VundleVim/Vundle.vim.git vim/bundle/vundle
fi

link() {
  from="$1"
  to="$2"
  echo "Linking '$from' to '$to'"
  rm -f $to
  ln -s "$from" "$to"
}

for location in $(find $HOME/.dotfiles -maxdepth 1 -name '*' ! -path '*.git'| sort); do
  name=$(basename $location)
  link "$HOME/.dotfiles/$name" "$HOME/.$name"
done
