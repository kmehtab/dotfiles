export ZSH="/home/kmehtab/.oh-my-zsh"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export VISUAL=nvim
export EDITOR="$VISUAL"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(git zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# alias
alias vim=lvim
alias ls='exa --icons --group-directories-first'
alias la='exa -a --long --no-user --git -h --icons --group-directories-first'
alias lc='exa -a --long --icons --group-directories-first'
alias lt='exa --icons --tree --level=3 --long'

# git alias
alias gd="git diff"
alias gl="git log"
alias gR="git reset --hard"
alias gCC="git clean -fd"
alias gp="git push"
alias gs="git status"
alias ga="git add ."
alias gA="git add "
alias gc="git commit -m"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

### RANDOM COLOR SCRIPT ###
#colorscript random
pfetch
