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
