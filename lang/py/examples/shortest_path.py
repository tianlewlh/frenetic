import base64
from ryu.lib.packet import packet_base,packet,ethernet,arp
from netkat import webkat
from netkat.syntax import *
import networkx as nx
from networkx.readwrite import json_graph
from ryu.ofproto.ether import ETH_TYPE_ARP
import redisnibjson # schema represented as JSON
#import redisnib # schema represented with redis hashes/sets

import time
import redis
import print_redis

"""Shortest Path Routing Based on Redis NIB"""

PROBE_INTERVAL = 5


#TODO: (nb) Could print_redis functionality be merged into redisnibjson?

nib = redisnibjson.RedisNib()  # using json for now
#nib = redisnib.RedisNib()



#TODO: (nb): There can be many optimizations to edge maintenance & computation
#            In which case, should cls store state of edges to efficiently update rather than reinit

    
def all_pairs_shortest_paths(edges):
    G = nx.Graph()
    G.add_edges_from(edges)
    path = nx.all_pairs_shortest_path(G)
    return path

def shortest_path(edges,src,dst):
    G = nx.Graph()
    G.add_edges_from(edges)
    try:
        path = nx.shortest_path(G,src,dst)
        return path
    except nx.exception.NetworkXError as e:
        print e
        return
    return path

# serialize to JSON if we wanted to compile to switch tables

def main():
    
    r = print_redis.connect()
    #get the real edges not in-memory

    print r.keys('*')

    while(True):
        #p = all_pairs_shortest_path(

        #two-nodes followed by an edge dictionary : 3 tuple

        edges = []

        for e in r.get('edges'):
            nodes = e.split("@")
            edges.append(nodes[0],nodes[1],r.get('edgeattr:%s' % e))

        p = shortest_path(edges,'1','00:00:00:00:00:02')
        if p:
            print p
        time.sleep(10)
            
if  '__main__':
    main()
