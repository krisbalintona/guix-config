#!/usr/bin/bash
# Import music release directory with wrtag.  See the wrtag docs for
# the wrtag API:
# https://github.com/sentriz/wrtag?tab=readme-ov-file#api

INPUT_PATH=$(echo "${SLSKD_SCRIPT_DATA}" | jq -r .localDirectoryName)

echo "Starting wrtag import of folder from: ${INPUT_PATH}"

# We don't use curl since wget is the only thing available in the
# slskd container.  This is different from the invocation in the wrtag
# documentation because when I used the one in from the documentation,
# authorization failed.  I'm not sure why.  The invocation below is
# the first one I tried that worked, so I've stuck with it.
wget -q -O/dev/null --auth-no-challenge \
     --user="" --password="${WRTAG_WEB_API_KEY}" \
     --post-data "" \
     "http://${WRTAG_WEB_URL}/op/move?path=${INPUT_PATH}"
