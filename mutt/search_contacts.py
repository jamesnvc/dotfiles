#!/usr/bin/env python2.7

import subprocess

CONTACTS_FORMAT_STR = '{}TOKEN%n'


def search_for(term):
    results = set()
    for email_tok in ['%e', '%we', '%oe']:
        try:
            output = subprocess.check_output([
                'contacts', '-HSf', CONTACTS_FORMAT_STR.format(email_tok), term
            ])
            addrs = [addr.replace('TOKEN', '\t') for addr in output.split('\n')
                    if addr and addr.find('@') != -1]
            results |= set(addrs)
        except subprocess.CalledProcessError:
            pass
    return results

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        sys.exit(0)
    addrs = search_for(sys.argv[1])
    if len(addrs):
        print 'EMAIL\tNAME'
        print '\n'.join(addrs)
