#!/usr/bin/env python

import sys
import subprocess

def usage():
    print "Usage: %s <library> <whitelist> <program>" % (sys.argv[0],)
    print
    print "  Specify a <whitelist> of symbols defined in <library> which"
    print "  <program> should be able to link against.  Any symbols"
    print "  defined in <library> not in the <whitelist> but used by"
    print "  <program> will generate an error message."
    print
    print "  <whitelist> is a text file with one symbol per line."
    print "  <library> is a C shared object."
    print "  <program> is a compiled C program which may or may not"
    print "     link against <library>."
    print

def read_whitelist(filename):
    return [line.strip() for line in open(filename)]

def fetch_library_symbols(filename):
    # Trim the first five lines to get rid of the header emitted by
    # objdump.
    esc_filename = filename.replace('"', '\\"')
    cmd = "objdump -TC \"%s\" | tail -n +5" % (esc_filename,)

    p = subprocess.Popen([cmd], shell=True, stdout=subprocess.PIPE)
    output = p.stdout.read()
    lines = output.splitlines()
    result = []

    # Some lines will be empty.
    for line in lines:
        parts = line.split()
        if len(parts) > 0:
            result.append(parts[-1])

    return result

def fetch_program_undefined_symbols(filename):
    p = subprocess.Popen(["nm", filename, "--undefined-only"], stdout=subprocess.PIPE)
    output = p.stdout.read()
    lines = output.splitlines()
    return [line.split()[-1] for line in lines]

def disallowed_symbols(whitelist, libsyms, prog_undef_syms):
    wl_set = set(whitelist)
    lib_set = set(libsyms)
    prog_set = set(prog_undef_syms)

    # We need to figure out which symbols in the program are defined
    # in the specified library:
    used_lib_syms = lib_set.intersection(prog_set)

    # Now determine which used symbols are not whitelisted.
    return list(used_lib_syms.difference(wl_set))

if __name__ == "__main__":
    if len(sys.argv) != 4:
        usage()
        exit(1)

    (lib_filename, wl_filename, prog_filename) = sys.argv[1:4]

    wl = read_whitelist(wl_filename)
    libsyms = fetch_library_symbols(lib_filename)
    progsyms = fetch_program_undefined_symbols(prog_filename)

    disallowed = disallowed_symbols(wl, libsyms, progsyms)

    if len(disallowed) == 0:
        exit(0)
    else:
        print "# Program '%s' uses the following disallowed symbols from library '%s':" % (
            prog_filename, lib_filename,)

        for sym in disallowed:
            print sym
        exit(1)
