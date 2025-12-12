if status is-login
    and status is-interactive
    # Ensure SHELL is properly.  Keychain uses this to infer how to
    # source a keychain command.  It may be incorrectly set in some
    # cases, e.g., calling fish manually from within another shell.
    # Taken from
    # https://github.com/fish-shell/fish-shell/issues/4583#issuecomment-1408657051
    set -lx SHELL fish
    # Call keychain, but:
    # 1. Use a running ssh-agent process if there is one.
    # 2. Don't print any messages.
    # 3. Add my personal SSH key but don't prompt for a password.
    #    (Means SSH key will be cached only if its password has been
    #    prompted for another way, e.g., a git push.)
    keychain --quick --quiet --eval id_ed25519 --noask | source
end
