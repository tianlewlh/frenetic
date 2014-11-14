import sys
import json
from tornado.httpclient import *
from tornado.concurrent import Future
from tornado import ioloop
from ryu.lib.packet import packet
import base64
from syntax import *

AsyncHTTPClient.configure("tornado.curl_httpclient.CurlAsyncHTTPClient")

async_client = AsyncHTTPClient()
sync_client = AsyncHTTPClient()

def _pkt_out(switch_id, port_id, packet):
    dict = { 'type' : 'packet_out',
             'data' : { 'switch_id' : repr(switch_id),
                        'port_id' : repr(port_id),
                        'packet' : base64.b64encode(packet),
                        'actions' : [] }}
    request = HTTPRequest("http://localhost:9000/pkt_out",
                          method='POST',
                          body=json.dumps(dict))
    response = async_client.fetch(request)
    return

def _update(policy):
    dict = { 'type' : 'policy',
             'data' : repr(policy) }
    request = HTTPRequest("http://localhost:9000/update",
                          method='POST',
                          body=json.dumps(dict))
    response = async_client.fetch(request)
    return


def _event(handler):
    def f(response):
        if response.error:
            print response
        else:
            body = response.body
            handler(json.loads(body))
    request = HTTPRequest("http://localhost:9000/event",
                          method='GET',
                          request_timeout=float(0))
    response = sync_client.fetch(request, callback=f)
    return

def start():
    ioloop.IOLoop.instance().start()

def periodic(f,n):
    cb = ioloop.PeriodicCallback(f,n * 1000)
    cb.start()
    return
# TODO: (vzy2, jnf) This is dead code.
def event_loop(handler):
    def handler_(event) :
        handler(event)
        ioloop.IOLoop.instance().add_callback(loop)
    def loop():
        event(handler_)
    loop()

class App:
    def __init__(self):
        pass
    def update(self, policy):
        _update(policy)

    def pkt_out(self, switch_id, port_id, packet):
        _pkt_out(switch_id, port_id, packet)
        
    def switch_up(self,switch_id):
        pass
    def switch_down(self,switch_id):
        pass
    def port_up(self,switch_id, port_id):
        pass
    def port_down(self,switch_id, port_id):
        pass
    def packet_in(self,switch_id, port_id, packet):
        pass
    def start(self):
        print "STARTING"
        def handler(event):
            typ = event['type']
            if typ == 'switch_up':
                print "START: SWITCH_UP"
                switch_id = event['switch_id']
                self.switch_up(switch_id)
            elif typ == 'switch_down':
                switch_id = event['switch_id']
                self.switch_down(switch_id)
            elif typ == 'port_up':
                switch_id = event['switch_id']
                port_id = event['port_id']
                self.port_up(switch_id, port_id)
            elif typ == 'port_down':
                switch_id = event['switch_id']
                port_id = event['port_id']
                self.port_down(switch_id, port_id)
            elif typ == 'packet_in':
                switch_id = event['switch_id']
                port_id = event['port_id']
                # TODO(jnf): this is expensive might not want to
                # decode and parse the packet here 
                bits = base64.b64decode(event['payload']['buffer'])
                pkt = packet.Packet(bits)
                self.packet_in(switch_id, port_id, pkt)
            else:
                pass
            ioloop.IOLoop.instance().add_callback(loop)
        def loop():
            _event(handler)
        loop()

class UpdateApp(App):
    def __init__(self,app,upd):
        self.app = app
        self.upd = upd
    def update(self, policy):
        self.upd(policy)
    def pkt_out(self, switch_id, port_id, packet):
        self.app.pkt_out(switch_id, port_id, packet)        
    def switch_up(self,switch_id):
        self.app.switch_up(switch_id)
    def switch_down(self,switch_id):
        self.app.switch_down(switch_id)
    def port_up(self,switch_id, port_id):
        self.app.port_up(switch_id, port_id)
    def port_down(self,switch_id, port_id):
        self.app.port_down(switch_id, port_id)
    def packet_in(self,switch_id, port_id, packet):
        self.app.packet_in(switch_id, port_id, packet)

def update1(app, policy):
    print "UPDATE1: " + str(policy)
    (pol1,pol2) = app.state
    app.state = (policy, pol2)
    app.update(policy | pol2)

def update2(app, policy):
    print "UPDATE2: " + str(policy)
    (pol1,pol2) = app.state
    app.state = (pol1, policy)
    app.update(pol1 | policy)

class UnionApp(App):
    def __init__(self, app1, app2):
        self.state = (drop(), drop())
        self.app1 = UpdateApp(app1, update1)
        self.app2 = UpdateApp(app2, update2)

    def switch_up(self,switch_id):
        print "UNION: SWITCH_UP"
        self.app1.switch_up(switch_id)
        self.app2.switch_up(switch_id)

    def switch_down(self,switch_id):
        self.app1.switch_down(switch_id)
        self.app2.switch_down(switch_id)

    def port_up(self,switch_id, port_id):
        self.app1.port_up(switch_id, port_id)
        self.app2.port_up(switch_id, port_id)

    def port_down(self,switch_id, port_id):
        self.app1.port_down(switch_id, port_id)
        self.app2.port_down(switch_id, port_id)

    def packet_in(self,switch_id, port_id, packet):
        self.app1.packet_in(switch_id, port_id, packet)
        self.app2.packet_in(switch_id, port_id, packet)
