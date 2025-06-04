# This line might be necessary depending on the value of SHELL.  See
# https://github.com/fish-shell/fish-shell/issues/4583#issuecomment-1408657051
set -lx SHELL fish

if status is-login
    and status is-interactive
    keychain --quick --quiet --ssh-allow-forwarded --eval id_ed25519 --noask | source
end
