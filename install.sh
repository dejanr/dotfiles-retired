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

for location in $(find $HOME/.dotfiles -maxdepth 1 -name '*' ! -path '*.git'| sort); do
  name=$(basename $location)
  link "$HOME/.dotfiles/$name" "$HOME/.$name"
done

if [ -e "$HOME/.spacemacs" ] || [ -e "$HOME/.spacemacs.d" ]; then
    echo "Spacemacs configuration alreay exists"
else
    ln -s "$DOTFILES/spacemacs.d" "$HOME/.spacemacs.d"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    echo "Configured and downloaded Spacemacs."
fi
