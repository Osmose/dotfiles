[[ $EMACS = t ]] && unsetopt zle

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="gentoo"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

setopt extendedglob

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/share/python/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:$HOME/tools/git-tools

export WORKON_HOME=$HOME/.virtualenvs
source $HOME/.virtualenvwrapper

export EDITOR=emacs

alias emacs='~/emacs'
alias ls='=ls -Gd ^(*~|*.pyc)'
alias ll='=ls -Gdl ^(*~|*.pyc)'

alias serve='python -m SimpleHTTPServer'

alias selenium_server='java -jar ~/misc/selenium-server-standalone-2.9.0.jar'

export PATH=$HOME/bin:$HOME/local/node/bin:$PATH
