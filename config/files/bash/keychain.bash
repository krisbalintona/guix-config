# Call keychain, but:
# 1. Use a running ssh-agent process if there is one.
# 2. Don't print any messages.
# 3. Add my personal SSH key but don't prompt for a password.
#    (Means SSH key will be cached only if its password has been
#    prompted for another way, e.g., a git push.)
eval `keychain --quick --quiet --eval id_ed25519 --noask`
