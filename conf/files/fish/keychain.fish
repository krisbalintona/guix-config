if status is-login
    and status is-interactive
    # Set SHELL, which keychain uses to infer how to source a keychain
    # command.  Taken from
    # https://github.com/fish-shell/fish-shell/issues/4583#issuecomment-1408657051
    set -lx SHELL fish
    keychain --quick --quiet --ssh-allow-forwarded --eval id_ed25519 --noask | source
end
