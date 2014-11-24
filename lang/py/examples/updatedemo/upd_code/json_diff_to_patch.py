# -*- encoding: utf-8 -*-
# json_delta.py: a library for computing deltas between
# JSON-serializable structures.
#
# Copyright 2012‒2014 Philip J. Roberts <himself@phil-roberts.name>.
# BSD License applies; see the LICENSE file, or
# http://opensource.org/licenses/BSD-2-Clause
#
# ###################################################
#
# Modified/Repurposed 2014 by KSaur (ksaur@umd.edu)
#
'''
Requires Python 2.7 or newer (including Python 3).
'''
import bisect, copy, sys, re
import json
import decode


__VERSION__ = '0.1'

try:
    _basestring = basestring
except NameError:
    _basestring = str

try:
    extra_terminals = (unicode, long)
except NameError:
    extra_terminals = ()

TERMINALS = (str, int, float, bool, type(None)) + extra_terminals
NONTERMINALS = (list, dict)
SERIALIZABLE_TYPES = ((str, int, float, bool, type(None),
                       list, tuple, dict) + extra_terminals)

# Some globals...
initdict = dict()   # parsed INITs
generatedfunctions = []  # generated function list
outfile = open("dsu.py", 'w') # the output file...


# ----------------------------------------------------------------------
# Main entry points


def diff(left_struc, right_struc, key=None):
    '''Compose a sequence of diff stanzas sufficient to convert the
    structure ``left_struc`` into the structure ``right_struc``.  (The
    goal is to add 'necessary and' to 'sufficient' above!).

    The parameter ``key`` is present because this function is mutually
    recursive with :py:func:`keyset_diff`.
    If set to a list, it will be prefixed to every keypath in the
    output.
    '''
    if key is None:
        key = []
    common = commonality(left_struc, right_struc)

    # We only need to worry about the first element in the array.
    # We are assuming for now that all elements have the same type,
    # and that they will be patched symmetrically.
    if ((type(left_struc) is list) and (type(right_struc) is list) and
        (len(left_struc) >0) and (len(right_struc) >0) ):
        if (len(key) != 0):
            ## mark the last item in "[]" to signal that it is a list
            key[len(key)-1] = [key[len(key)-1]]

        #print("Truncating Array....")
        left_struc=left_struc[0]
        right_struc=right_struc[0]
    if common is True:
        # We don't care about values changing, just keys.
        # This will ignore the values and just continue processing if
        # the type has changed, or if it's a container (list/dict)
        if((type(left_struc) is not type(right_struc)) or
            (type(left_struc) is list) or (type(left_struc) is dict)):
            my_diff = this_level_diff(left_struc, right_struc, key, common)
        else:
            my_diff = []
    else:
        if type(left_struc) in NONTERMINALS:
            my_diff = keyset_diff(left_struc, right_struc, key)
        else:
            print "RETURNING"
            my_diff = [] # no objects here...

    if key == []:
        if len(my_diff) > 1:
            my_diff = sort_stanzas(my_diff)
    return my_diff


# ----------------------------------------------------------------------
# Functions for computing normal diffs.

def compute_keysets(left_seq, right_seq):
    '''Compare the keys of ``left_seq`` vs. ``right_seq``.

    Determines which keys ``left_seq`` and ``right_seq`` have in
    common, and which are unique to each of the structures.  Arguments
    should be instances of the same basic type, which must be a
    non-terminal: i.e. list or dict.  If they are lists, the keys
    compared will be integer indices.

    Returns:
        Return value is a 3-tuple of sets ``({overlap}, {left_only},
        {right_only})``.  As their names suggest, ``overlap`` is a set
        of keys ``left_seq`` have in common, ``left_only`` represents
        keys only found in ``left_seq``, and ``right_only`` holds keys
        only found in ``right_seq``.

    Raises:
        AssertionError if ``left_seq`` is not an instance of
        ``type(right_seq)``, or if they are not of a non-terminal
        type.

    >>> compute_keysets({'foo': None}, {'bar': None})
    (set([]), set(['foo']), set(['bar']))
    >>> compute_keysets({'foo': None, 'baz': None}, {'bar': None, 'baz': None})
    (set(['baz']), set(['foo']), set(['bar']))
    >>> compute_keysets(['foo', 'baz'], ['bar', 'baz'])
    (set([0, 1]), set([]), set([]))
    >>> compute_keysets(['foo'], ['bar', 'baz'])
    (set([0]), set([]), set([1]))
    >>> compute_keysets([], ['bar', 'baz'])
    (set([]), set([]), set([0, 1]))
    '''
    assert isinstance(left_seq, type(right_seq))
    assert type(left_seq) in NONTERMINALS

    if type(left_seq) is dict:
        left_keyset = set(left_seq.keys())
        right_keyset = set(right_seq.keys())
    else:
        left_keyset = set(range(len(left_seq)))
        right_keyset = set(range(len(right_seq)))

    overlap = left_keyset.intersection(right_keyset)
    left_only = left_keyset - right_keyset
    right_only = right_keyset - left_keyset

    #print ("======= overlap / left / right ==========")
    #print (overlap)
    #print (left_only)
    #print (right_only)
    #print ("=========================================")
    return (overlap, left_only, right_only)

def keyset_diff(left_struc, right_struc, key):
    '''Return a diff between ``left_struc`` and ``right_struc``.

    It is assumed that ``left_struc`` and ``right_struc`` are both
    non-terminal types (serializable as arrays or objects).  Sequences
    are treated just like mappings by this function, so the diffs will
    be correct but not necessarily minimal.

    This function probably shouldn't be called directly.  Instead, use
    :func:`udiff`, which will call :func:`keyset_diff` if appropriate
    anyway.
    '''
    out = []
    (o, l, r) = compute_keysets(left_struc, right_struc)
    out.extend([[key + [k]] for k in l])
    # The INIT gets pulled later. This is a placeholder for the next step.
    out.extend([[key + [k], "INIT"] for k in r])
    for k in o:
        sub_key = key + [k]
        out.extend(diff(left_struc[k], right_struc[k],
                        sub_key))
    return out

def this_level_diff(left_struc, right_struc, key=None, common=None):
    '''Return a sequence of diff stanzas between the structures
    left_struc and right_struc, assuming that they are each at the
    key-path ``key`` within the overall structure.'''
    out = []
    # Always compute the keysets, return the diff.
    (o, l, r) = compute_keysets(left_struc, right_struc)
    for okey in o:
        if left_struc[okey] != right_struc[okey]:
            out.append([key[:] + [okey], right_struc[okey]])
    for okey in l:
        out.append("DEL"+[key[:] + [okey]])
    for okey in r:
        out.append("ADD"+[key[:] + [okey], right_struc[okey]])
    return out

def commonality(left_struc, right_struc):
    '''Return True if structs are the same type or terminals. Else False.
    '''
    if type(left_struc) is not type(right_struc):
        return True
    if type(left_struc) in TERMINALS:
        return True
    else:
        return False

def split_deletions(diff):
    '''Return a tuple of length 3, of which the first element is a
    list of stanzas from ``diff`` that modify objects (dicts when
    deserialized), the second is a list of stanzas that add or change
    elements in sequences, and the second is a list of stanzas which
    delete elements from sequences.'''
    objs = [x for x in diff if isinstance(x[0][-1], _basestring)]
    seqs = [x for x in diff if isinstance(x[0][-1], int)]
    assert len(objs) + len(seqs) == len(diff), diff
    seqs.sort(key=len)
    lengths = [len(x) for x in seqs]
    point = bisect.bisect_left(lengths, 2)
    return (objs, seqs[point:], seqs[:point])

def sort_stanzas(diff):
    '''Sort the stanzas in ``diff``: node changes can occur in any
    order, but deletions from sequences have to happen last node
    first: ['foo', 'bar', 'baz'] -> ['foo', 'bar'] -> ['foo'] ->
    [] and additions to sequences have to happen
    leftmost-node-first: [] -> ['foo'] -> ['foo', 'bar'] ->
    ['foo', 'bar', 'baz'].

    Note that this will also sort changes to objects (dicts) so that
    they occur first of all, then modifications/additions on
    sequences, followed by deletions from sequences.
    '''
    # First we divide the stanzas using split_deletions():
    (objs, mods, dels) = split_deletions(diff)
    # Then we sort modifications of lists in ascending order of last key:
    mods.sort(key=lambda x: x[0][-1])
    # And deletions from lists in descending order of last key:
    dels.sort(key=lambda x: x[0][-1], reverse=True)
    # And recombine:
    return objs + mods + dels


# ----------------------------------------------------------------------

# Basic script functionality

def generate_upd(thediff):
    print len(thediff)
    function = ""
    getter = ""
    for l in thediff:
        # l[0] is the key to where to modify the data
        # l[1] is 'INIT' if adding, otherwise there is no l[1]
        assert(len(l) in (1,2))
        keys = l[0]
        assert(len(keys)>0)
        if (keys[0] != function):
            outfile.write("\ndef update_" + keys[0] + "(rediskey, jsonobj):\n")
            function = keys[0]
            generatedfunctions.append(keys[0])
        # get the item to modify
        pos = 'e'  # for code generation.
                   # This is the first variable name and we'll increment it
        tabstop = "    "
        codeline = tabstop + pos + " = jsonobj"
        for s in keys[0:(len(keys)-1)]:
            if (type(s) is str):
                codeline += ".get(\'" + s + "\')"
            else: # arrays
                # if array isn't the leaf
                nextpos = chr(ord(pos) + 1) # increment the variable name
                codeline += ".get(\'" + s[0] + "\')\n"
                codeline += tabstop + "assert(" + pos + " is not None)\n"
                codeline += tabstop + "for " + nextpos +" in " + pos + ":"
                tabstop = tabstop + "    "
                pos = nextpos
                if (s != keys[(len(keys)-2)]):
                    nextpos = chr(ord(pos) + 1)
                    codeline += "\n" + tabstop + nextpos + " = " + pos
                    pos = nextpos
                # TODO There are probably several scenarios this leaves out?
        if (getter != codeline):
            outfile.write(codeline+"\n")
            outfile.write(tabstop + "assert(" + pos + " is not None)\n")
            getter = codeline

        # adding
        if (len(l) == 2):
            try:
                init = initdict[str(keys)]
            except:
                print "+----------------------------------------------------------+"
                print "| Expected an INIT command for \'" + str(keys[(len(keys)-1)]) + "\'"
                print "|   at \'"  + str(keys)  + "\'"
                print "+----------------------------------------------------------+"
                sys.exit(-1)
            # Replace $out's with the variable to assign to
            vartoassign = pos+"[\'" + keys[len(keys)-1] + "\']"
            # whoa. where has this function been my whole life?
            init = init.replace("$out", vartoassign)
            init = init.replace("|", "\n"+tabstop)
            outfile.write(tabstop + init+"\n")
        # deleting.
        else:
            outfile.write(tabstop + "del "+pos+"[\'" + (keys[len(keys)-1]) + "\']\n")


def regextime(dslf):
    # Load up the init file
    dslfile = open(dslf, 'r')
    ## DBG, but remember this eats the file.
    #for line in dslfile:
    #    print line,
    #    initdict

    patterns =  ['(INIT)\\s+(\[.*\])\\s?:\\s?\{(.*)\}',     #INIT [...] : {...}
                 '(TODO\\s+)(\[.*\])\\s?:\\s?{\\s?([a-zA-Z0-9]+)\\s?}']  # -> syntax TODO

    def extract_from_re(estr):
        for p in patterns:
            if re.match(p,estr) is not None:
                cmd_re = re.compile(p)
                cmd = cmd_re.search(estr)
                print cmd_re.groups
                for i in range(1,cmd_re.groups+1):
                    print "Group " +str(i) + " = " +  cmd.group(i)
                return cmd
            else:
                print "FAIL"

    for line in dslfile:
        line = line.rstrip('\n')
        # parse multiline cmds
        while '}' not in line and line is not "":
            tmp = next(dslfile, None)
            if tmp is not None:
                line += '|' + tmp.rstrip('\n')
            else: # EOF
                break
        print "=========================================\n\nline = " + line
        curr = extract_from_re(line)
        if curr is not None:
            print "found " + str(len(curr.groups())) + " groups"
            # add to INIT dictionary if init
            #print curr.group(1) #DBG
            if (curr.group(1) == 'INIT'):
                assert(len(curr.groups()) is 3)
                initdict[curr.group(2)] = curr.group(3)


    print "\nINIT dict is : "
    print initdict


def bulkload(f, jsonarr):
    for line in f:
        # A file may contain mulitple JSON objects with unknown size.
        # This code will load up all the JSON objects by reading from file
        # until one is successfully parsed.  Continuges until EOF.
        # http://stackoverflow.com/questions/20400818/
        while line is not None:
            try:
                jfile = json.loads(line,object_hook=decode.decode_dict)
                break
            except ValueError:
                # Not yet a complete JSON value
                tmp = next(f, None)
                if tmp is not None:
                    line +=tmp
                else: # EOF
                    break
        print "loaded:"
        print jfile
        jsonarr.append(jfile)



def main():
    assert (len(sys.argv) in (3, 4)), '\n\nUsage is: \"python json_diff_to_patch.py \
    <json1> <json2> (optional <initfile>)\".\n Ex: \"python json_diff_to_patch.py \
    ../example_json/sample1.json ../example_json/sample2.json\"'

    file1 = open(sys.argv[1], 'r')
    file2 = open(sys.argv[2], 'r')
    jsonarray1 = []
    jsonarray2 = []

    bulkload(file1, jsonarray1)
    bulkload(file2, jsonarray2)
    print "ARRRAY IS:"
    print jsonarray1

    assert len(jsonarray1) == len(jsonarray2), \
     "Files should contain the same number of json templates..."


    dslfile = None
    if len(sys.argv) is 4:
        dslfile = regextime(sys.argv[3])


    for json1, json2 in zip(jsonarray1, jsonarray2):

        thediff = diff(json1, json2)
        print ("\nTHE DIFF IS: (len " + str(len(thediff)) + ")")
        print (thediff)

        # generate the functions for objects that needs modifying
        generate_upd(thediff)

        # check for arrays containing objects. If so, grab the first inner obj
        if (type(json1) is list) and (len(json1)>0) and (type(json1[0]) is dict):
            json1 = json1[0]

       # generate the functions as placeholders for outer objects if no diff
        if type(json1) is dict:
            for k in json1.keys():
                if k not in generatedfunctions:
                    outfile.write("\ndef update_" + k + "(jsonobj):\n    ()\n")



if __name__ == '__main__':
    main()
