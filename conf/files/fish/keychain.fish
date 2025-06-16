if status is-login
    and status is-interactive
    # Ensure SHELL is properly.  Keychain uses this to infer how to
    # source a keychain command.  It may be incorrectly set in some
    # cases, e.g., calling fish manually from within another shell.
    # Taken from
    # https://github.com/fish-shell/fish-shell/issues/4583#issuecomment-1408657051
    set -lx SHELL fish
    keychain --quick --quiet --ssh-allow-forwarded --eval id_ed25519 --noask | source
end
