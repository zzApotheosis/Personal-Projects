# Helper functions
function prepend_path --argument new_path
    if not test -d "$new_path"
        return
    end
    if not contains -- $new_path $PATH
        set -gx PATH $new_path $PATH
    end
end

function append_path --argument new_path
    if not test -d "$new_path"
        return
    end
    if not contains -- $new_path $PATH
        set -gx PATH $PATH $new_path
    end
end

function warn --argument filename --argument lineno
    for msg in $argv[3..-1]
        printf "[%s:%d] %s\n" "$filename" "$lineno" "$msg" 1>&2
    end
    # printf "[%s:%d] $argv[3..-1]\n" "$filename" "$lineno" 1>&2
end

function find_ssh_auth_sock
    set _result 0

    if test -z "$SSH_AUTH_SOCK"
        set -gx SSH_AUTH_SOCK $(find $HOME/.ssh -type s -print -quit 2> /dev/null)
        set _result 1
    end
    if test -z "$SSH_AUTH_SOCK"
        set -gx SSH_AUTH_SOCK $(find /tmp -name 'agent.*' -print -quit 2> /dev/null)
        set _result 1
    end

    return $_result
end
