"""
Base class for redis nib. Meant as a drop-in replacement for
several functions from networkx-1.9.1/networkx/classes/digraph.py

JSON Version.

TODO: These return ints instead of strings.  (Ex: "outport" is now a string.)
      I think this make the most sense, so if we go with this version,
      we need to fixup discovery.py.

Current Schema:  (note that node/edge attributes are application-specific,
                  as defined by each application's logic/usage.)
-------------------------------------------------------------------------------
Key Name         | JSON Object or Array
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
"nodes"          | JSON array of all nodes in graph.
                 | (one "nodes" key per graph)
-----------------|-------------------------------------------------------------
"edges"          | JSON array of all edges in graph.
                 | (one "edges" key per graph)
-----------------|-------------------------------------------------------------
"nodeports:(n)"  | JSON array of all ports for a node with name "n".
                 | (one key per node)
                 |   [ 80, 8080]
-----------------|-------------------------------------------------------------
"nodeattr:(n)"   | JSON object of variable node attributes for node "n".
                 | (one key per node).
                 | Attributes determined/set by app logic.
                 | - For discovery.py, these attributes are defined as:
                 |   nodeattr: {"switch": "device"}
-----------------|-------------------------------------------------------------
"edgeattr:(u@v)" | JSON object of variable edge attributes for directed edge
                 | from node "u" to node "v".  (one key per edge)
                 | - For discovery.py, these attributes are defined as:
                 |      edgeattr: { "inport": 80, "outport": 8675}
-----------------|-------------------------------------------------------------

"""

import redis
import sys
import re
import json

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

    ######  Helper functions to manipulate JSON  #######
    ######  TODO: sepearate module?              #######

    def append_to_arr(self, key, value):
        """ Append 'value' to the array belonging to 'key', if it does not
            exist. (JSON arrays convert to python lists and viseversa.)
        """
        value_str = self.r.get(key)
        if value_str is None:
            arr = [] # TODO (ks): what's the best python syntax for null append?
            arr.append(value)
            self.r.set(key, json.dumps(arr))
        else:
            vals = json.loads(value_str)
            # Append value to json array
            if value not in vals:
                try:
                    vals.append(value)
                    self.r.set(key, json.dumps(vals))
                except: # TODO (ks): how to best handle errors?
                    print 'The value arg could not be appended to the array.'

    def exists(self, key, value):
        """ Check to see if 'value' exists in the array belonging to 'key' """
        value_str = self.r.get(key)
        if value_str is not None:
            vals = json.loads(value_str)
            # Check for value from json array
            if value in vals:
                return True
        return False

    def get_arr(self, key):
        """ For 'key', return the corresponding array as a list"""
        value_str = self.r.get(key)
        if value_str is not None:
            return json.loads(value_str)
        else:
            return []

    def del_from_arr(self, key, value):
        """ Delete 'value' from the array belonging to 'key', if it exists.
            (JSON arrays convert to python lists and viseversa.)
        """
        value_str = self.r.get(key)
        if value_str is not None:
            vals = json.loads(value_str)
            # Delete value from json array
            if value in vals:
                try:
                    vals.remove(value)
                    self.r.set(key, json.dumps(vals))
                except: # TODO (ks): how to best handle errors?
                    print 'The value arg could not be deleted from the array.'

    ######  END Helper functions to manipulate JSON  #######

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

        """
        
        # Must cast to string here (allows simpler application logic)
        n = str(n)

        # If attributes, store them in JSON with key ('nodeattr:'+n)
        if attr:
            # From digraph.py
            if attr_dict is None:
                attr_dict=attr
            try:
                attr_dict.update(attr)
            except: # TODO (ks): how to handle errors?
                print 'The attr_dict argument must be a dictionary.'
            # Store the attributes in addition to storing the node
            self.r.set('nodeattr:'+n, json.dumps(attr_dict))
        # Add n to the JSON array of nodes.
        self.append_to_arr('nodes', n)

    def add_port(self, n, p):
        """
        Add port p to switch sw.
        Note that adding a port to a node creates it.  (So no need to
        call "nib.node[n]['ports'] = set()".)

        TODO not tested:
        >>> nib.add_port(event['switch_id'], event['port_id'])
        >>> # Replaces (hopefully, not tested!!):
        >>> # nib.node[event['switch_id']]['ports'].add(event['port_id'])

        How redis is used here:
          Each node 'n' is stored in a JSON array of ports with key
          'nodeports:'+n for each node

        """
        
        n = str(n)
        
        # First make sure the node exists
        s = self.exists('nodes', n)
        if s is True:
            self.append_to_arr('nodeports:'+n, p)

    def del_port(self, n, p):
        """ Deletes port p from node n
        """
        self.del_from_arr('nodeports:'+n, p)

    def node_ports(self, n):
        """
        Return the set of all ports (as ints) for node n, or empty set
        ([]) if no ports found.

        Example:
        >>> for port in nib.node_ports(node):
        >>> #replaces "for port in nib.node[node]['ports']:"

        """
        # First make sure the node exists
        s = self.exists('nodes', n)
        if s is False:
            return set([])
        # Get the list of existing ports for node n.
        nodeports_str = self.r.get('nodeports:'+n)
        if nodeports_str is None:
            return set([])
        else:
            return set(json.loads(nodeports_str))

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
    
        n = str(n)

        # First make sure the node exists
        s = self.exists('nodes', n)
        if s is False:
            return None
        else:
            if attr_lookup is not None:
                # The node exists. Now lookup the requested attributes
                attr_dict_str = self.r.get('nodeattr:'+n)
                if attr_dict_str is not None:
                    return (json.loads(attr_dict_str).
                       get(attr_lookup))
                else:
                    return None
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
        
        n = str(n)
        
        # Remove node n.  TODO (ks): do we need to report if not found?
        self.del_from_arr('nodes', n)
        # Remove node attributes, if they exist
        self.r.delete('nodeattr:'+n)
        # Remove node ports, if they exist
        self.r.delete('nodeports:'+n)
        # Remove all edges involving this node, if they exist
        # TODO: optimize
        for e in self.edges():
            if((e[0] == n) or (e[1] == n)): 
                self.remove_edge(e[0], e[1])

    # http://stackoverflow.com/questions/21283042/keep-empty-data-keys-in-redis
    def nodes(self):
        """ Return a list of all the nodes. """
        n = self.get_arr('nodes')
        if n is not None:
           return n
        else:
           return []

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


        Examples
        --------
        >>> nib.add_edge('sw1','sw2')
        >>> nib.add_edge('sw2','sw4',outport=8675,inport=80)

        """

        u = str(u)
        v = str(v)

        # If attributes, store them in a JSON object with key ('edgeattr:'+n)
        if attr:
            # From digraph.py
            if attr_dict is None:
                attr_dict=attr
            try:
                attr_dict.update(attr)
            except: # TODO (ks): how to handle errors?
                print 'The attr_dict argument must be a dictionary.'
            # Store the attributes in addition to storing the node
            self.r.set('edgeattr:' + u + '@' + v, json.dumps(attr_dict))
        # Add u->v to the set of edges.
        self.append_to_arr('edges', u + '@' + v)

        # "The nodes u and v will be automatically added if they are
        # not already in the graph." (from digraph.py, we can change)
        if not self.exists ('nodes', u):
            self.add_node(u)
        if not self.exists ('nodes', v):
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

         >>> if(nib.edge('sw2', 'sw4', 'outport') == 8675): # test attrib
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
        edgename = u + '@' + v
        if self.exists('edges', edgename) is False:
            return None
        else:
            if attr_lookup is not None:
                # The edge exists. Now lookup the requested attributes
                attr_dict_str = self.r.get('edgeattr:'+edgename)
                if attr_dict_str is not None:
                    return (json.loads(attr_dict_str).
                        get(attr_lookup))
                else:
                    return None
            else:
                return edgename

    def edges(self):
        """ Return a list of all the edge as tuples. """
        # (ks): Do we need (data=True)?  To get attributes as well?

        # We must convert a set of concat'ed strings back in to tuples
        l = self.get_arr('edges')
        if l is None:
            return []
        edgelist = []

        # Currently not making any assumptions about node names, other than 
        #   they don't contain the character '@'
        for estr in l:
            edge = estr.split("@")
            assert (len(edge) == 2)
            in_node = str(edge[0])
            out_node = str(edge[1])
            edgelist.append((in_node,out_node))

        return edgelist

    def remove_edge(self, u, v):
        """Remove the edge between nodes u and v."""
        # TODO (ks): do we need to report if not found?
        self.del_from_arr('edges', u + '@' + v)
        # Remove node attributes, if they exist
        self.r.delete('edgeattr:' + u + '@' + v)

    # TODO (ks): Implement this stuff (and others...?):
    # More attribute related things
    # More graph computations...maybe we can leverage networkx


########## Tests ##################

def test_add_del_node(nib):
    # setup
    nib.r.flushall() # wipe previous
    assert(nib.nodes() == [])
    nib.add_node('sw1',device='switch')
    nib.add_node('sw2')

    # Test add:
    # assert that the count for set 'nodes' is 2.
    node_arr_str = nib.r.get('nodes')
    vals = json.loads(node_arr_str)
    assert (len(vals) == 2), 'Failed to add nodes'
    # assert that device attributes set for sw1
    attr_dict_str = nib.r.get('nodeattr:sw1')
    assert (attr_dict_str is not None), 'Couldn\'t find node sw1'
    assert (json.loads(attr_dict_str).get('device') == \
        'switch'), 'Failed to set node attrib'

    # Test remove:
    nib.remove_node('sw1')
    # Test that sw1 was removed from 'nodes' array
    assert (nib.exists('nodes', 'sw1') is False), 'Failed to delete sw1'
    # Test that key 'nodeattr:sw1' was removed from redis
    assert (nib.r.exists('nodeattr:sw1') is False), 'Failed to del sw1 attrib'

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

    # test add_port to a legit node
    nib.add_port('sw1', 80)
    portlist = json.loads(nib.r.get('nodeports:sw1'))
    assert (len(portlist) is 1), 'Found wrong number of ports'
    assert ((80 in portlist) is True), 'Failed to add port 80'

    # test add_port to a fake node
    nib.add_port('sw234234', 80)
    assert (nib.r.exists('nodeports:sw234234') is False), \
        'Failed to return False for adding port to fake switch'

    # test returning the set of nodes
    assert (nib.node_ports('sw1') == set([80])),\
        'Found unexpected set of ports for sw1'

    # test deleting a port
    nib.del_port('sw1', 80)
    assert (nib.exists('sw1', 80) is False), 'Failed to delete port 80'
    portlist = json.loads(nib.r.get('nodeports:sw1'))
    assert ((80 in portlist) is False), \
       'Failed to delete port 80 from set sw1'

    # test that deleting a node deletes its ports
    nib.add_port('sw1', 443)
    portlist = json.loads(nib.r.get('nodeports:sw1'))
    assert (len(portlist) is 1), 'Found wrong number of ports'
    assert ((443 in portlist) is True), 'Failed to add port 80'
    nib.remove_node('sw1')
    assert (nib.r.get('nodeports:sw1') is None), 'Failed to delete portlist'


def test_add_del_edges(nib):
    # setup
    nib.r.flushall() # wipe previous
    assert(nib.edges() == [])
    nib.add_edge('sw1','sw2')
    nib.add_edge('sw2','sw4',outport=8675,inport=80)
    nib.add_edge('sw1','sw5')
    nib.add_edge('sw5','sw2')

    # Test that adding edge adds both nodes
    assert (nib.node('sw1') is not None), 'Couldn\'t find node sw1 (add edge)'
    assert (nib.node('sw2') is not None), 'Couldn\'t find node sw2 (add edge)'

    # Test add edge:
    # assert that the count for set 'edges' is 4.
    edges_arr_str = nib.r.get('edges')
    vals = json.loads(edges_arr_str)
    assert (len(vals) == 4), 'Failed to add edges'

    # assert basic edge add
    assert (nib.exists('edges', 'sw1@sw2') is True), \
        'Failed to add edge sw1@sw2'
    # assert that device attributes set for sw2->sw4
    attr_dict_str = nib.r.get('edgeattr:sw2@sw4')
    assert (attr_dict_str is not None), 'Couldn\'t find edge attrib sw2@sw4'
    assert (json.loads(attr_dict_str).get('outport') == \
        8675), 'Failed to set edge attrib'

    # Test that there are no dangling edges after a node is removed
    assert(nib.exists('edges', 'sw1@sw5') is True), \
        'Failed to add edge 1@5 for node sw5'
    assert(nib.exists('edges', 'sw1@sw5') is True), \
        'Failed to add edge 5@2 for node sw5'
    nib.remove_node('sw5')
    assert(nib.exists('edges', 'sw1@sw5') is False), \
        'Failed to delete edge 1@5 for deleted node sw5'
    assert(nib.exists('edges', 'sw1@sw5') is False), \
        'Failed to delete edge 5@2 for deleted node sw5'

    # Test remove edge:
    nib.remove_edge('sw1', 'sw2')
    # Test that sw1@sw3 was removed from 'edges' array
    assert (nib.exists('nodes', 'sw1@sw2') is False), \
        'Failed to delete sw1@sw2'
    nib.remove_edge('sw2', 'sw4')
    # Test that key 'edgeattr:sw2@sw4' was removed from redis
    assert (nib.r.exists('edgeattr:sw2@sw4') is False), \
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
    assert (nib.edge('sw2', 'sw4', 'outport') == 8675), \
        'Error getting edge attrib outport for sw2->sw4'

def test_json_helper_functions(nib):
    # setup
    nib.r.flushall() # wipe previous

    nib.append_to_arr('testkey', 8080)
    l = json.loads(nib.r.get('testkey'))
    assert (len(l) is 1), 'Did not find the right number of appended elements'
    assert ((8080 in l) is True), 'Did not find the right port in appended elm'
    assert (nib.exists('testkey', 8080) is True), 'Exists should have been True'

    nib.append_to_arr('testkey', 8080)
    l = json.loads(nib.r.get('testkey'))
    assert (len(l) is 1), 'Appended duplicate element'

    nib.append_to_arr('testkey', 80)
    l = json.loads(nib.r.get('testkey'))
    assert (len(l) is 2), 'Did not append as expected'

    full_list = nib.get_arr('testkey')
    assert (len(full_list) is 2), 'Did not convert JSON array to full list'

    nib.del_from_arr('testkey', 80)
    l = nib.get_arr('testkey')
    assert (len(l) is 1), 'Did not delete as expected'
    assert ((80 in l) is False), 'Should have deleted 80 from list'
    assert (nib.exists('testkey', 80) is False), 'Exists should have been False'

    nib.del_from_arr('testkey', 8080)
    l = nib.get_arr('testkey')
    assert (len(l) is 0), 'Did not delete as expected, should be empty'

def main():
    nib = RedisNib()
    test_add_del_node(nib)
    test_json_helper_functions(nib)
    test_get_node(nib)
    test_ports(nib)
    test_add_del_edges(nib)
    test_get_edges(nib)
    print 'All assertions pass'

if __name__ == '__main__':
    main()
