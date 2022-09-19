alias ls='ls --color=auto'
alias rf='readlink -f'

alias tmux='tmux -2'
alias ta='tmux attach -t'
alias tnew='tmux new -s'
alias tls='tmux ls'
alias tkill='tmux kill-session -t'
alias diff2="diff -y --suppress-common-lines"

mkcdir () {mkdir "$1" && cd "$1"}

# from https://remysharp.com/2018/08/23/cli-improved

alias pping='prettyping --nolegend'
alias ncdu='ncdu --color dark -rr -x'
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias RR=radian


sfw() {
    targ=${3:- $2}
    ssh $1 -NL "$targ":localhost:$2
}
alias cat=bat

sview () {samtools view $1 | less -S}

cdl () {cd "$1" && ls -la}
zless () {zcat "$1" | less -S}
