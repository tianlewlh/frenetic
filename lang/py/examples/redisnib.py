"""Base class for redis nib. Meant as a drop-in replacement for
   several functions from networkx-1.9.1/networkx/classes/digraph.py """

import redis
import sys


# Stores a Redis hash of nodes
class RedisNib:

    def __init__(self, host=None, port=None):
        """ Connect to redis. Default is localhost, port 6379.
            (Redis must be running.)
        """
        if host is None:
            self.host = 'localhost'
        else:
            self.host = host
        if port is None:
            self.port = 6379
        else:
            self.port = port
        self.r = redis.StrictRedis(self.host, self.port, db=0)
        try:
            self.r.ping()
        except self.r.ConnectionError as e:
            print(e)
            sys.exit(-1)
        self.r.flushall() # wipe previous entries from database

    # TODO (ks) : add a set of ports for each host.
    def add_node(self, n, attr_dict=None, **attr):
        """Add a single node n and update node attributes.

        Parameters (verbatim from networkx/classes/digraph.py:add_node)
        ----------
        n : node
            A node can be any hashable Python object except None.
        attr_dict : dictionary, optional (default= no attributes)
            Dictionary of node attributes.  Key/value pairs will
            update existing data associated with the node.
        attr : keyword arguments, optional
            Set or change attributes using key=value.

        >>> nib.add_node('sw1',device='switch')
        >>> nib.add_node('sw2')

        How redis is used here:
            - The nodes are stored in a redis set 'nodes' (One set to store all
              the nodes.)  Sets don't allow duplicates, multiple calls to add
              do nothing.  (We can change this however we like.)
            - The attributes (if present) for each node are stored in a redis
              hash entry (which is basically a python dictionary) with the
              prefix 'nodeattr'+n (one hash entry per node attributes
        """
        # If no attributes, just store the node
        if not attr:
            self.r.sadd('nodes', n)
        else:
            # From digraph.py
            if attr_dict is None:
                attr_dict=attr
            try:
                attr_dict.update(attr)
            except: # TODO (ks): how to handle errors?
                print 'The attr_dict argument must be a dictionary.'
            # Store the attributes in addition to storing the node
            self.r.hmset('nodeattr:'+n, attr_dict)
            self.r.sadd('nodes', n)

    def node(self, n, attr_lookup=None):
        """ Gets node n with optional attributes.
            Note that the syntax for 'node' is different from networkx

            Example:
            >>> if(nib.node(node, 'device') == 'switch'):
            >>> # With networkx, this would have been:
            >>> #if nib.node[node]['device'] == 'switch':

            Returns None if unable to find node, or unable to find node
            with requested attribute """
        s = self.r.smembers('nodes') # return the set of all nodes
        # First make sure the node exists
        if n not in s:
            return None
        else:
            # The node exists. Now lookup the requested attributes
            return self.r.hget('nodeattr:'+n, attr_lookup)

    def remove_node(self, n):
        """Remove node n.
        Parameters
        ----------
        n : node
           A node in the graph
        >>> nib.remove_node(sw)
        """
        # Remove node n.  TODO (ks): do we need to report if not found?
        self.r.srem('nodes', n)
        # Remove node attributes, if they exist
        self.r.delete('nodeattr:'+n)
        # TODO (ks): remove set of ports for each host when it exists.

    def nodes(self):
        """ Return a list of all the nodes. """
        return list(self.r.smembers('nodes')) # return the set of all nodes

    # TODO (ks): Implement this stuff from Nick's code
    # nib.edges(data=True)
    # nib.node.has_key(sw)
    # nib.add_edge(sw,host_id,outport=pt,inport=0)
    # nib.node[sw]['ports'] = set()  # add additional data structure
    # nib.node[event['switch_id']]['ports'].add(event['port_id'])


########## Tests ##################

def test_add_del(nib):
    #setup
    nib.r.flushall() # wipe previous
    nib.add_node('sw1',device='switch')
    nib.add_node('sw2')

    # Test add:
    # assert that the count for set 'nodes' is 2.
    assert (nib.r.scard('nodes') == 2), 'Failed to add nodes'
    # assert that device attributes set for sw1
    assert (nib.r.hget('nodeattr:sw1', 'device') ==
       'switch'), 'Failed to set attrib'

    # Test remove:
    nib.remove_node('sw1')
    assert (nib.r.sismember('nodes', 'sw1') is False), 'Failed to delete sw1'
    nib.remove_node('sw2')
    assert (nib.r.exists('nodeattr:sw1') is False), 'Failed to del sw2 attrib'

def test_get_node(nib):
    #setup
    nib.r.flushall() # wipe previous
    nib.add_node('sw1',device='switch')
    nib.add_node('sw2')

    # test for unadded node. Could switch to assertIsNotNone if using py2.7
    assert (nib.node('sw7777') is None), 'Found phantom node...'
    # test for correct device setting
    assert (nib.node('sw1', 'device') == 'switch'), 'Found dangling node attrib'

    # test for returning a set of the nodes
    l = ['sw1', 'sw2']
    assert ((nib.nodes() == l) is True), 'Failed to get correct set of nodes'

def main():
    nib = RedisNib()
    test_add_del(nib)
    test_get_node(nib)
    print 'All assertions pass'

if __name__ == '__main__':
    main()
