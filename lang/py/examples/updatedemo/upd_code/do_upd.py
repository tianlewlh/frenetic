"""

Call all of the generated functions for the redis database.

Usage: python doupd.py

This assumes the update functions are in a file called dsu.py (todo)

With help from:
http://stackoverflow.com/questions/3061/
calling-a-function-of-a-module-from-a-string-with-the-functions-name-in-python

"""
import sys
import json
import decode
import redis


def connect(host=None, port=None):
    """ Connect to redis. Default is localhost, port 6379.
        (Redis must be running.)
    """
    if host is None:
        host = 'localhost'
    if port is None:
        port = 6379
    r = redis.StrictRedis(host, port, db=0)
    try:
        r.ping()
    except r.ConnectionError as e:
        print(e)
        sys.exit(-1)
    return r

# TODO...redis objects

def main():

    r = connect()
    # load up the file to get all functions (like dlsym with Kitsune)
    m = __import__ ('dsu')
    # Get all the keys for redis
    keys = r.keys('*');
    print "Printing \'" + str(len(keys)) + "\' keys:"
    for currkey in keys:
        redisval = None
        print "key: " + currkey
        redisval = r.get(currkey)
        print "value: |" + redisval + "|"


        # Make sure everything is loaded
        assert redisval is not None, ("could not find value for" + currkey)
        print type(redisval)
        jsonkey = json.loads(redisval, object_hook=decode.decode_dict)

        # check for arrays containing objects. If so, grab the first inner obj
        if (type(jsonkey) is list) and (len(jsonkey)>0) and (type(jsonkey[0]) is dict):
            jsonkey = jsonkey[0]
        print "LOADED:",
        print jsonkey
        # Looping in case the user puts more than one JSON entry per key
        if type(jsonkey) is dict:
            for o in jsonkey.keys():
                # Create the function name 
                funcname = "update_"+o
                func = getattr(m,funcname)
                assert func is not None, ("Could not find function for" + funcname)

                # Call the function for the current key and current jsonsubkey
                func(currkey, jsonkey)

                # Now serialize it back, then write it back to redis.  
                # (Note that the key was modified in place.)
                modedkey = json.dumps(jsonkey)
                r.set(currkey, modedkey)
            

if __name__ == '__main__':
    main()
