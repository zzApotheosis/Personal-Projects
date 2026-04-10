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
