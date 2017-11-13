alias ec='emacsclient -t'
alias eg='emacsclient -c'
alias rdolphin='dolphin > /dev/null 2>&1 ./ &'

alias sshorchest='ssh vp76@orchestra.med.harvard.edu -t zsh'

alias ls='ls --color=auto'
alias rf='readlink -f'

alias zshconfig="vim ~/.zshrc"

alias tmux='tmux -2'
alias ta='tmux attach -t'
alias tnew='tmux new -s'
alias tls='tmux ls'
alias tkill='tmux kill-session -t'
alias diff2="diff -y --suppress-common-lines"

mkcdir () {mkdir "$1" && cd "$1"}
