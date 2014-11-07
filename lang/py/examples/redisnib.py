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

    def add_node(self, n, attr_dict=None, **attr):
        """Add a single node n and update node attributes.

        Parameters (similar to networkx/classes/digraph.py:add_node)
        ----------
        n : node
            A node string representation of a node.
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
              prefix 'nodeattr'+n (one hash entry per node attributes)
        """
        # If attributes, store them in a redis hash as ('nodeattr:'+n)
        if attr:
            # From digraph.py
            if attr_dict is None:
                attr_dict=attr
            try:
                attr_dict.update(attr)
            except: # TODO (ks): how to handle errors?
                print 'The attr_dict argument must be a dictionary.'
            # Store the attributes in addition to storing the node
            self.r.hmset('nodeattr:'+n, attr_dict)
        # Add n to the set of nodes.
        self.r.sadd('nodes', n)

    def add_port(self, n, p):
        """
        Add port p to switch sw.
        Note that adding a port to a set creates it.  (So no need to
        call "nib.node[n]['ports'] = set()".)

        TODO not tested:
        >>> nib.add_port(event['switch_id'], event['port_id'])
        >>> # Replaces (hopefully, not tested!!):
        >>> # nib.node[event['switch_id']]['ports'].add(event['port_id'])

        How redis is used here:
          Each node 'n' has a set of ports stored as a redis set indexed
          by 'nodeports:'+n

        """
        # First make sure the node exists
        s = self.r.smembers('nodes') # return the set of all nodes
        if n not in s:
            return None
        return self.r.sadd('nodeports:'+n, p)

    def del_port(self, n, p):
        """ Deletes port p from switch n """
        # First make sure the node exists
        s = self.r.smembers('nodes') # return the set of all nodes
        if n not in s:
            return None
        return self.r.srem('nodeports:'+n, p)

    def node_ports(self, n):
        """
        TODO/WARNING: Returns a set of _strings_ not ints.  Which is preferred?

        Return the set of all ports (as strings) for node n, or empty set
        ([]) if no ports found.

        Example:
        >>> for port in nib.node_ports(node):
        >>> #replaces "for port in nib.node[node]['ports']:"

        """
        return self.r.smembers('nodeports:'+n)

    def node(self, n, attr_lookup=None):
        """
        Parameters
        ----------
        n : node
            A node string representation of a node.
        attr_lookup :
            Lookup and return attributes using key=value.

        Example:
        >>> if(nib.node(node, 'device') == 'switch'):
        >>> # With networkx, this would have been:
        >>> #if nib.node[node]['device'] == 'switch':

        >>> if (nib.node(sw1) is not None):
        >>> # replaces: nib.node.has_key(sw)

        Returns:
           - None if unable to find node.
           - Node n's value mapping of 'attr_lookup' for if provided
           - Node n's name if node found and no 'attr_lookup' provided
             (Used to test if exists.)
             (TODO is there something better to return for the last case?)
        """
        # First make sure the node exists
        s = self.r.smembers('nodes') # return the set of all nodes
        if n not in s:
            return None
        else:
            if attr_lookup is not None:
                # The node exists. Now lookup the requested attributes
                return self.r.hget('nodeattr:'+n, attr_lookup)
            else:
                return n

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
        # (ks): Do we need (data=True) like networkx? Get attributes as well?
        return list(self.r.smembers('nodes')) # return the set of all nodes

    def add_edge(self, u, v, attr_dict=None, **attr):
        """Add an edge between u and v.

        The nodes u and v will be automatically added if they are
        not already in the graph.

        Edge attributes can be specified with keywords.

        Parameters (similar to networkx/classes/digraph.py:add_edge)
        ----------
        u,v : nodes
            Nodes represented as strings.
        attr_dict : dictionary, optional (default= no attributes)
            Dictionary of edge attributes.  Key/value pairs will
            update existing data associated with the edge.
        attr : keyword arguments, optional
            Edge data can be assigned using keyword arguments.

        Notes
        -----
        Adding an edge that already exists updates the edge data.

        How redis is used here:
            - The nodes are stored in a redis set 'edges' (One set to store
              all. The edge is stored as concat strings (u+':'+v), to be
              (un)marshalled as necessary.  (Directional.)

            - The attributes (if present) for each edge are stored in a redis
              hash entry (which is basically a python dictionary) with the
              prefix 'edgeattr'+u+':'+v (one hash entry per edge attributes)

        Examples
        --------
        >>> nib.add_edge('sw1','sw2')
        >>> nib.add_edge('sw2','sw4',outport=8675,inport=80)

        """

        # If attributes, store them in a redis hash as ('edgeattr:'+n)
        if attr:
            # From digraph.py
            if attr_dict is None:
                attr_dict=attr
            try:
                attr_dict.update(attr)
            except: # TODO (ks): how to handle errors?
                print 'The attr_dict argument must be a dictionary.'
            # Store the attributes in addition to storing the node
            self.r.hmset('edgeattr:' + u + ':' + v, attr_dict)
        # Add u->v to the set of edges.
        self.r.sadd('edges', u + ':' + v)

        # "The nodes u and v will be automatically added if they are
        # not already in the graph." (from digraph.py, we can change)
        s = self.r.smembers('nodes') # return the set of all nodes
        if u not in s:
            self.add_node(u)
        if v not in s:
            self.add_node(v)

    def edge(self, u, v, attr_lookup=None):
        """
        Parameters
        ----------
        u, v : (nodes, seeking edge u->v)
            String representation of nodes u and v.
        attr_lookup :
            Lookup and return attributes using key=value.

        Example:

         >>> if(nib.edge('sw2', 'sw4', 'outport') == '8675'): # test attrib
         >>>     # .....something
         >>> nib.edge('sw1', 'sw2') is not None) # test for edge presence

        Returns:
           - None if unable to find edge u->v.
           - Edge u->v's value mapping of 'attr_lookup' for if provided
           - Edge u->v's name if edge found and no 'attr_lookup' provided
             (Used to test if exists.)
             (TODO is there something better to return for the last case?)
        """
        # First make sure the edge exists
        s = self.r.smembers('edges') # return the set of all edges
        edgename = u + ':' + v
        if (edgename) not in s:
            return None
        else:
            if attr_lookup is not None:
                # The node exists. Now lookup the requested attributes
                return self.r.hget('edgeattr:'+edgename, attr_lookup)
            else:
                return edgename

    def edges(self):
        """ Return a list of all the edge as tuples. """
        # (ks): Do we need (data=True)?  To get attributes as well?

        # We must convert a set of concat'ed strings back in to tuples
        l = list(self.r.smembers('edges'))
        edgelist = []
        for estr in l: # l = ['sw2:sw4', ....
            if ':' not in estr:
                print 'Error in parsing edge name' + str
                return None
            if len(l) is not 2:
                print 'Error in parsing edge name list' + str
                return None
            pair = estr.split(':')
            edgelist.append( (pair[0], pair[1]) )
        return edgelist

    def remove_edge(self, u, v):
        """Remove the edge between nodes u and v."""
        # TODO (ks): do we need to report if not found?
        self.r.srem('edges', u + ':' + v)
        # Remove node attributes, if they exist
        self.r.delete('edgeattr:' + u + ':' + v)

    # TODO (ks): Implement this stuff (and others...?):
    # More attribute related things
    # More graph computations...maybe we can leverage networkx


########## Tests ##################

def test_add_del_node(nib):
    # setup
    nib.r.flushall() # wipe previous
    nib.add_node('sw1',device='switch')
    nib.add_node('sw2')

    # Test add:
    # assert that the count for set 'nodes' is 2.
    assert (nib.r.scard('nodes') == 2), 'Failed to add nodes'
    # assert that device attributes set for sw1
    assert (nib.r.hget('nodeattr:sw1', 'device') ==
       'switch'), 'Failed to set node attrib'

    # Test remove:
    nib.remove_node('sw1')
    assert (nib.r.sismember('nodes', 'sw1') is False), 'Failed to delete sw1'
    nib.remove_node('sw2')
    assert (nib.r.exists('nodeattr:sw1') is False), 'Failed to del sw2 attrib'

def test_get_node(nib):
    # setup
    nib.r.flushall() # wipe previous
    nib.add_node('sw1',device='switch')
    nib.add_node('sw2')

    # test for unadded node. Could switch to assertIsNotNone if using py2.7
    assert (nib.node('sw7777') is None), 'Found phantom node...'
    # test that you can lookup an expected node without attributes
    assert (nib.node('sw1') is not None), 'Couldn\'t find node sw1'
    # test for correct attribute setting
    assert (nib.node('sw1', 'device') == 'switch'), 'Found dangling node attrib'

    # test for returning a set of the nodes
    l = ['sw1', 'sw2']
    assert ((nib.nodes() == l) is True), 'Failed to get correct set of nodes'

def test_ports(nib):
    # setup
    nib.r.flushall() # wipe previous
    nib.add_node('sw1',device='switch')

    # test expected empty
    assert (nib.node_ports('nodeports:sw1') == set([])),\
        'Found unexpected port, expected empty'
    assert (nib.r.sismember('nodeports:sw1', 80) == False), \
        'Found unexpected port 80'

    # test add_port to a legit node
    assert (nib.add_port('sw1', 80) == 1), 'Failed add new port'
    assert (nib.r.sismember('nodeports:sw1', 80) == True), 'Failed to add port'

    # test add_port to a fake node
    assert (nib.add_port('sw234234', 80) is None), \
        'Failed to return None for adding port to fake switch'

    # test returning the set of nodes
    assert (nib.node_ports('sw1') == set(['80'])),\
        'Found unexpected set of ports for sw1'

    # test deleting a port
    assert (nib.del_port('sw1', 80) == 1), 'Failed to delete port 80'
    assert (nib.r.sismember('nodeports:sw1', 80) == False), \
       'Failed to delete port 80 from set sw1'

def test_add_del_edges(nib):
    # setup
    nib.r.flushall() # wipe previous
    nib.add_edge('sw1','sw2')
    nib.add_edge('sw2','sw4',outport=8675,inport=80)

    # Test that adding edge adds both nodes
    assert (nib.node('sw1') is not None), 'Couldn\'t find node sw1 (add edge)'
    assert (nib.node('sw2') is not None), 'Couldn\'t find node sw2 (add edge)'

    # Test add edge:
    # assert that the count for set 'edges' is 2.
    assert (nib.r.scard('edges') == 2), 'Failed to add nodes'
    # assert basic edge add
    assert (nib.r.sismember('edges', 'edge:sw1:sw2') is False), \
        'Failed to add edge sw1:sw2'
    # assert that device attributes set for sw2->sw4
    assert (nib.r.hget('edgeattr:sw2:sw4', 'outport') ==
       '8675'), 'Failed to set edge attrib'

    nib.edges()
    # Test remove edge:
    nib.remove_edge('sw1', 'sw2')
    assert (nib.r.sismember('nodes', 'sw1:sw2') is False), \
        'Failed to delete sw1:sw2'
    nib.remove_edge('sw2', 'sw4')
    assert (nib.r.exists('edgeattr:sw2:sw4') is False), \
        'Failed to del sw2 attrib'

def test_get_edges(nib):
    # setup
    nib.r.flushall() # wipe previous
    nib.add_edge('sw1','sw2')
    nib.add_edge('sw2','sw4',outport=8675,inport=80)

    # test for returning a set of the nodes
    l = [('sw1','sw2'), ('sw2','sw4') ]
    # convert to sets for comparison of two (unordered) lists
    assert (set(nib.edges()) == set(l)) is True, \
        'Failed to get correct set of edges'

    # test for unadded edge. Could switch to assertIsNotNone if using py2.7
    assert (nib.edge('sw1', 'sw9') is None), 'Found phantom edge...'
    # test that you can lookup an expected edge without attributes
    assert (nib.edge('sw1', 'sw2') is not None), 'Couldn\'t find edge sw1->sw2'
    # test for correct attribute setting
    assert (nib.edge('sw2', 'sw4', 'outport') == '8675'), \
        'Error getting  edge attrib outport for sw2->sw4'

def main():
    nib = RedisNib()
    test_add_del_node(nib)
    test_get_node(nib)
    test_ports(nib)
    test_add_del_edges(nib)
    test_get_edges(nib)
    print 'All assertions pass'

if __name__ == '__main__':
    main()
