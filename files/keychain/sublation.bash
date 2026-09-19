# Call keychain, but:
# 1. Use a running ssh-agent process if there is one.
# 2. Don't print any messages.
eval `keychain --quick --quiet --noask`
