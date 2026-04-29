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
