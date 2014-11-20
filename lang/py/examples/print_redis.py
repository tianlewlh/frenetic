"""
 Prints all the values of redis.

 Usage: python print_redis.py

 Notes:
     - You can call r.flushall() to wipe all keys in running redis.
     - If you find lingering keys after restarting redis, check for a file
       called "dump.rdb" in your redis server directory and delete it.
       (Depending on how redis is shut down, it sometimes serializes itself
       to disk)

"""
import redis

def connect(host=None, port=None):
    """ Connect to redis. Default is localhost, port 6379.
        (Redis must be running.)
    """
    r = None
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

def print_all_keys():
    r = connect()
    keys = r.keys('*');
    print "Printing \'" + str(len(keys)) + "\' keys:"
    for key in keys:
        typ = r.type(key)
        print "key (" + typ + "): " + key
        print "value: ",
        if typ == 'string':
            print r.get(key)
        elif typ == 'hash':
            print r.hgetall(key)
        elif typ == 'zset':
            print r.zrange(key, 0, -1)
        elif typ == 'set':
            print r.smembers(key)
        elif typ == 'list':
            print r.lrange(key, 0, -1)
        print "---"


def main():
    print_all_keys()

if __name__ == '__main__':
    main()
