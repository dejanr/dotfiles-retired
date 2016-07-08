link() {
  from="$1"
  to="$2"
  echo "Linking '$from' to '$to'"
  rm -f $to
  ln -s "$from" "$to"
}

if [[ -d vim/bundle/vundle ]]; then
  curl -fLo vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim
fi

if [[ ! -d ~/.nix-defexpr ]]; then
  mkdir -p ~/.nix-defexpr
  link "$HOME/.dotfiles/nixpkgs" "$HOME/.nix-defexpr/nixpkgs"
fi

for location in $(find $HOME/.dotfiles -maxdepth 1 -name '*' ! -path '*.git'| sort); do
  name=$(basename $location)
  link "$HOME/.dotfiles/$name" "$HOME/.$name"
done
