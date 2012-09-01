#!/usr/bin/env python2.7

import subprocess

CONTACTS_FORMAT_STR = '{}TOKEN%n'


def search_for(term):
    results = set()
    for email_tok in ['%e', '%we', '%oe']:
        output = subprocess.check_output([
            'contacts', '-HSf', CONTACTS_FORMAT_STR.format(email_tok), term
        ])
        addrs = [addr.replace('TOKEN', '\t') for addr in output.split('\n')
                 if addr and addr.find('@') != -1]
        results |= set(addrs)
    return results

if __name__ == '__main__':
    import sys
    addrs = search_for(sys.argv[1])
    print 'EMAIL\tNAME'
    print '\n'.join(addrs)
