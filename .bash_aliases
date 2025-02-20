alias ls='ls --color=auto'
alias ll='lsd -l'
alias la='lsd -la'
alias rf='readlink -f'

alias zshconfig="vim ~/.zshrc"

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

alias snowpack='yarn run snowpack'
alias bat='bat --style="changes,header"'
alias e=micro

sview () {samtools view $1 | less -S}

cdl () {cd "$1" && ls -la}
zless () {zcat "$1" | less -S}

alias open=xdg-open

# SSH port forwarding helper function
# Creates SSH tunnels for single ports, port ranges, or comma-separated port lists
# Example usage:
#   sfw server.com 8888          # Forward single port 8888
#   sfw server.com 8888-8890     # Forward port range 8888 to 8890
#   sfw server.com 8888,9999     # Forward multiple specific ports
#   sfw server.com 8888 9999     # Forward local 8888 to remote 9999
sfw() {
    # Check if we have enough arguments
    if [ "$#" -lt 2 ]; then
        echo "Usage: sfw server.address port_spec [target_port_spec]"
        echo "  port_spec: Single port (8888), range (8888-8890), or comma-separated list (8888,9999,9515)"
        return 1
    fi

    local server=$1      # SSH server address
    local -a source_ports    # Array for local ports
    local -a target_ports    # Array for remote ports

    # Parse port specification
    if [[ $2 =~ '^[0-9]+-[0-9]+$' ]]; then
        # Handle port range (e.g., 8888-8890)
        local start_port=${2%-*}
        local end_port=${2#*-}
        source_ports=({$start_port..$end_port})
    elif [[ $2 =~ '^[0-9]+(,[0-9]+)*$' ]]; then
        # Handle comma-separated list (e.g., 8888,9999,9515)
        source_ports=(${(s:,:)2})
    else
        # Single port
        source_ports=($2)
    fi

    # Handle target ports if specified
    if [ -n "$3" ]; then
        if [[ $3 =~ '^[0-9]+-[0-9]+$' ]]; then
            local start_port=${3%-*}
            local end_port=${3#*-}
            target_ports=({$start_port..$end_port})
        elif [[ $3 =~ '^[0-9]+(,[0-9]+)*$' ]]; then
            target_ports=(${(s:,:)3})
        else
            target_ports=($3)
        fi
    else
        # If no target ports specified, use source ports
        target_ports=($source_ports)
    fi

    # Validate port counts match
    if [ ${#source_ports} -ne ${#target_ports} ]; then
        echo "Error: Number of source ports does not match number of target ports"
        return 1
    fi

    # Build SSH command with all port forwards
    local ssh_cmd="ssh $server -N"
    local i
    for (( i=1; i <= ${#source_ports}; i++ )); do
        ssh_cmd+=" -L ${source_ports[$i]}:localhost:${target_ports[$i]}"
    done

    # Execute SSH command
    echo "Starting port forwarding..."
    echo "$ssh_cmd"
    eval "$ssh_cmd"

    # Set up trap to handle Ctrl+C gracefully
    trap 'echo "Port forwarding stopped."; return 0' INT TERM
}
