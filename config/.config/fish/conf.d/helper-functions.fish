# Helper functions
function prepend_path --argument new_path
    if not contains -- $new_path $PATH
        set -gx PATH $new_path $PATH
    end
end
