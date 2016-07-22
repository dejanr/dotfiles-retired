These are config files i use to configure all the apps i am using.
You may also be interested in my [nixos configuration](https://github.com/dejanr/nixos) how my os is provisioned.
So with this two repositories i have fully deterministic and reproducible os envirionment, setting up new machine involves runing nixos-rebuild and installing dotfiles.


## Installation

```bash
    git clone git://github.com/dejanr/dotfiles ~/.dotfiles
    cd ~/.dotfiles
    ./install.sh
```

## Vim bundles

To manage vim dependencies Use :PlugInstall inside vim.
