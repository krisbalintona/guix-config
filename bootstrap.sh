#!/bin/bash

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# * Email
MAIL_ROOT="$(notmuch config list | grep database.mail_root | cut -d'=' -f2-)"

# ** Notmuch and lieer
PERSONAL_MAILDIR=$MAIL_ROOT/personal
UNI_MAILDIR=$MAIL_ROOT/uni
GMI_CONFIG_FILES=$SCRIPT_DIR/conf/files/gmi
GMI_CREDENTIAL_FILES="/run/user/$(id -u)/secrets/gmi-credentials"

echo "       Bootstrapping notmch"

# Make directories lieer expects
echo "Making email directory structure in $MAIL_ROOT..."
mkdir -p $PERSONAL_MAILDIR/mail/{cur,new,tmp}
mkdir -p $UNI_MAILDIR/mail/{cur,new,tmp}
echo "Done!"

# FIXME 2025-07-02: Not sure if this is necessary if the assumption is
# that I've already run guix home reconfigure, which creates my
# notmuch config
# Create notmuch config if there isn't one already.
if [ -z "$(notmuch config list 2>/dev/null)" ]; then
    echo "Notmuch configuration not found. Running notmuch setup..."
    notmuch setup
else
    echo "Notmuch configuration found."
fi
# Create (empty) notmuch database (.notmuch directory in
# MAIL_ROOT).  This is necessary since gmi pull (for some reason)
# expects the exists of the database.
if [ ! -d "$MAIL_ROOT/.notmuch" ]; then
    echo "Notmuch database not found. Creating..."
    notmuch new
else
    echo "Notmuch database found."
fi

# FIXME 2025-07-02: Consider doing this symlinking in my guix home
# configuration, how I symlink .authinfo from the files decrypted by
# sops-guix.
# Symlink lieer setting and credential files
echo "Symlinking lieer setting files into email directories..."
# Configuration files
ln -sf $GMI_CONFIG_FILES/personal-config.json $PERSONAL_MAILDIR/.gmailieer.json
ln -sf $GMI_CONFIG_FILES/uni-config.json $UNI_MAILDIR/.gmailieer.json
echo "Symlinking lieer credential files into email directories..."
# Credentials
ln -sf $GMI_CREDENTIAL_FILES/personal $PERSONAL_MAILDIR/.credentials.gmailieer.json
ln -sf $GMI_CREDENTIAL_FILES/uni $UNI_MAILDIR/.credentials.gmailieer.json
echo "Done!"

# Initial gmi pulls
echo "Pulling emails and populating notmuch database..."
(cd $PERSONAL_MAILDIR
 gmi pull -f) &
(cd $UNI_MAILDIR
 gmi pull -f) &
wait
notmuch new
echo "Done!"

# ** l2md
echo "       Bootstrapping l2md"

# Make directory l2md expects
echo "Making email l2md directory in $MAIL_ROOT..."
mkdir $MAIL_ROOT/l2md/

# Pull in all l2md emails
echo "Pulling in all l2md emails..."
l2md

# Populate notmuch database
notmuch new

# ** Notmuch tag backup (notmuch dump)
# Back up notmuch tags after initial email pulls, in case the tags are
# incorrect.  This dump file can be used to correct any erroneous tag
# changes.
echo "       Creating notmuch tag dump..."
TIMESTAMP="$(date +"%Y%m%dT%H%M%S")"
TAG_DUMP_FILENAME="$TIMESTAMP-notmuch_tags_dump"
TAG_DUMP_PATH=$HOME/Downloads/$TAG_DUMP_FILENAME
notmuch dump --format=batch-tag --output=$TAG_DUMP_PATH
echo "Done! You can find the file at $TAG_DUMP_PATH."

# * Done!
echo "       All done!"
