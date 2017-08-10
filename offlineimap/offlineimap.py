"""Script to use in offlineimap to read encrypted mail passwords

To use, include the lines `pythonfile = <path to this file>` under the [general]
section and `remotepasseval = mailpassword("account-name")` under the remote
repository section of your offlineimaprc.

To create the encrypted files, create a file in
`~/.passwd/account-name` containing your password, then run
`gpg -e -r $gpg_key_id ~/.password/account-name` then delete the unencrypted file
"""
import os
import subprocess


def mailpassword(account):
    acct = os.path.basename(account)
    path = "/home/james/.passwd/%s.gpg" % acct
    args = ["gpg", "--use-agent", "--quiet", "--batch", "-d", path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ""
