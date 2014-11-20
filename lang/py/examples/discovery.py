import base64
from ryu.lib.packet import packet,ethernet,arp
from netkat import webkat
from netkat.syntax import *
import networkx as nx
from networkx.readwrite import json_graph
from ryu.ofproto.ether import ETH_TYPE_ARP
import redisnibjson # schema represented as JSON
#import redisnib # schema represented with redis hashes/sets

"""Topology Discovery"""

###
# Globals
#
# PROBE_INTERVAL: frequency to send LLDP-like probes in seconds
#
# nib: dictionary with switches as keys and list of ports as values
#      (jnf): this will have to change obviously when you're reprsenting
#             the actual graph, and more.
#             consider using networkx?
##

PROBE_INTERVAL = 5

# (nb): Arbitrarily assigned ethtype to (switch) discovery packets
ETH_TYPE_DISCOVERY_PACKET = 0x3366

nib = redisnibjson.RedisNib()  # using json for now
#nib = redisnib.RedisNib()

##
# Helper functions
##

# Extract Ethernet frame from
# Ryu packet
def get_ethernet(pkt):
    for p in pkt:
        if p.protocol_name == 'ethernet':
            return p

# NetKAT policy that diverts packets to the controller
def controller():
    return modify("port", "http")

###
# Policy
###
def policy():
    return (filter(test("ethTyp", ETH_TYPE_DISCOVERY_PACKET)) >> controller()) | (filter(test("ethTyp", ETH_TYPE_ARP)) >> controller())

##
# Emit Probes
##
def probe():
    print '=== PROBE ==='
    for node in nib.nodes():
        if(nib.node(str(node),'device') == 'switch'):
            #ports are no longer being associated
            for port in nib.node_ports(node):
                # (nb): NOTE: Force node,port into dst,src fields of Ethernet packet. Temporary only!
                dp = ethernet.ethernet(dst=int(node),src=int(port),ethertype=0x3366)
                p = packet.Packet()
                p.add_protocol(dp)
                p.serialize()
                webkat.pkt_out(int(node),int(port),p.data)
    print nib.edges() #(data=True)
    return

class TopologyDiscovery(webkat.App):
    def switch_up(self,switch_id):
	if(nib.node(switch_id) is not None):
            pass
        else:
            nib.add_node(str(switch_id),device='switch')
	webkat.update(policy())
    def switch_down(self,switch_id):
	nib.remove_node(str(switch_id))
	webkat.update(policy())
    def port_up(self,switch_id,port_id)
	nib.add_port(str(switch_id),port_id)
	webkat.update(policy())
    def port_down(self,switch_id,port_id):
	nib.add_port(str(switch_id),port_id)
        webkat.update(policy()) #TODO: No longer needed?
    def packet_in(self,switch_id,port_id,packet):
	p = get_ethernet(packet)
	if p.ethertype == ETH_TYPE_ARP:
           host_id = "h%s" % (str(switch_id) + "-" + str(port_id))
	   nib.add_node(str(host_id),device='host')
	   nib.add_edge(str(switch_id),str(host_id),outport=port_id,inport=0)
	   nib.add_edge(str(host_id),str(switch_id),outport=0,inport=port_id)
        elif p.ethertype == ETH_TYPE_DISCOVERY_PACKET:
	   # (nb): Remove ryu.packet's MAC string format
	   src_sw = int((p.dst).replace(':',''),16)
   	   src_port = int((p.src).replace(':',''),16)
   	   nib.add_edge(str(src_sw),str(switch_id),outport=src_port,inport=port_id)
	   nib.add_edge(str(switch_id),str(src_sw),outport=port_id,inport=src_port)	
	pass
	# print "NIB: %s" % json_graph.node_link_data(nib)
	webkat.update(policy())

def main():
    TopologyDiscovery().start()
    webkat.periodic(probe,PROBE_INTERVAL)
    webkat.start()

if __name__ == '__main__':
    main()
