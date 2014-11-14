from ryu.lib.packet import packet
import base64
from netkat import webkat
import networkx as nx
from netkat.syntax import *
import learning

"""Ethernet Learning switch"""


conns = nx.DiGraph()

##
# Helper functions
##

def get_tcp(pkt):
    for p in pkt:
        if p.protocol_name == 'tcp':
            return p
def get_ip(pkt):
    for p in pkt:
        if p.protocol_name == 'ipv4':
            return p


def controller():
    return modify("port", "http")



##
# Learning switch functions
##

known_pred = false()


def monitor(packet):
    global known_pred
    p1 = get_tcp(packet)
    tcpSrc = p1.src_port
    tcpDst = p1.dst_port
    p2 = get_ip(packet)
    ipSrc = p2.src
    ipDst = p2.dst
    conns.add_node(ipSrc, device = 'host')
    conns.add_node(ipDst, device = 'host')
    conns.add_edge(ipSrc, ipDst, src = tcpSrc, dst = tcpDst)
    print conns
    host_pred = test('ipSrc', ipSrc) & test('ipDst', ipDst) & test('tcpSrcPort', tcpSrc) & test('tcpDstPort', tcpDst)
    known_pred = known_pred | host_pred
    
def policy():
    return filter(~known_pred) >> controller()

class MonitorApp(webkat.App):

    def packet_in(self,switch_id, port_id, packet):
        monitor(packet)
        self.update(policy())

if __name__ == '__main__':
    print "---WELCOME TO MONITOR---"
    webkat.UnionApp(learning.LearningApp(), MonitorApp()).start()
    webkat.start()
