if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi

if which rbenv 2> /dev/null > /dev/null; then eval "$(rbenv init -)"; fi
