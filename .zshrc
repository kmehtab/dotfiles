# GPG_TTY
export GPG_TTY=$(tty)
# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="/Users/kmehtab/.oh-my-zsh"
export PATH=/usr/local/bin:/usr/local/sbin:${PATH}
export PATH="$HOME/.emacs.d/bin:$PATH"


ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(git zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh
# aliases
alias zshrc="nvim ~/.zshrc"
alias vimrc="nvim ~/.vimrc"
alias nvimrc="nvim ~/.config/nvim/init.vim"
alias vim="nvim"
alias download="cd /Volumes/Downloads\ HD"
alias study="cd /Volumes/Study\ HD"
alias phone="cd /Volumes/Phone\ HD"
alias projects="cd /Volumes/Study\ HD/Development/projects"
alias repo="cd /Volumes/Study\ HD/Development/repo"
alias development="cd /Volumes/Study\ HD/Development"
# alias ls='colorls --sf'
# alias la='colorls -A --sf'
# alias lc='colorls -lA --sd'
# alias lt='colorls --t=2 --sf'
alias ls='exa --icons --group-directories-first'
alias la='exa -a --long --no-user --git -h --icons --group-directories-first'
alias lc='exa -a --long --icons --group-directories-first'
alias lt='exa --icons --tree --level=3 --long'

#Python path
export PATH="/usr/local/opt/python@3.9/bin:$PATH"
export ANDROID_HOME=/Users/$USER/Library/Android/sdk
export PATH=${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools
export PATH=$HOME/.local/bin:$PATH
export LC_ALL=en_US.UTF-8
export PATH="/usr/local/opt/openjdk@11/bin:$PATH"
GITSTATUS_LOG_LEVEL=DEBUG

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
#neofetch

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# android studio path
export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
