# Provides fuzzy completion for commands and recursive file/directory
# search for paths.  Just bind the command fzf_complete.
#
# Dependencies:
# - fd
# - fzf
#
# Behavior:
# - Operate asynchronously: fd is called after fzf is started, and the
#   user can make fzf queries as fd returns its output
# - For path-like tokens (starting with ./ ~/ or /), performs
#   recursive file search from the contextually relevant directory
#   using fd
# - For empty or non-path tokens, uses Fish's built-in completion
#   system
# - Preserves ~ path abbreviation from original token
# - Replaces the current token with the selected completion
# - Completions are relative file paths
function fzf_complete
    # Get command line context
    set -l cmdline (commandline -poc)
    set -l cmd $cmdline[1]
    # We use -x to make the token a shell expansion rather than a
    set -l current_token (commandline -xt)[-1]
    set -l fzf_height "40%"
    set -l result
    # echo "" >&2                                # Debug
    # echo "Current token: '$current_token'" >&2 # Debug

    # Check if current token looks like a path (starts with ./, ~/, or /)
    if string match -qr '^[./~]|/' -- "$current_token"

        # path dirname and path basename return desired values when
        # current_token is not a directory.  When it is, we must set
        # search_dir and file_basename speically
        if test -d $current_token
            set search_dir $current_token
            set file_basename ""
        else
            set search_dir (path dirname -- $current_token)
            set file_basename (path basename -- $current_token)
        end
        # echo "Search dir: '$search_dir'" >&2       # Debug
        # echo "File basename: '$file_basename'" >&2 # Debug

        # Perform search when search_dir is a string
        if test -n $search_dir
            # abbreviate $HOME with ~
            set replace_tilde "string replace --regex '^$HOME' '~'"
            set fd_cmd "fd --hidden --exclude '*/*.jj' . '$search_dir' | $replace_tilde"
            # echo "fd_cmd: '$fd_cmd'" >&2 # Debug

            # The --bind option with start:reload:... tells fzf to
            # begin a reload event on fzf's startup, with that event
            # calling fd_cmd.  Effectively, fzf has no candidates
            # initially, calling fd at start time and allowing
            # (asynchronous) querying as fd runs in the background
            set result (fzf --height "$fzf_height" --query "$file_basename" --bind "start:reload:$fd_cmd")
            # echo "Result: '$result'" >&2 # Debug
        end
    else
        # Token is either empty (then: regular prompt for directory)
        # or a non-path (e.g., flag or option): use Fish's built-in
        # completion system
        set result (complete -C(commandline -cp) | fzf --height $fzf_height)
    end

    # Repaint prompt after fzf exits (handles Escape key cleanup)
    commandline -f repaint
    # Replace current token with selected completion
    if test -n "$result"
        commandline --current-token --replace -- $result
    end
end
