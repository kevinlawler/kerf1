#Dual 2.7/3.5: Python2.7 + Python3.5
#import kerf
#kerf = kerf.KerfConnection('localhost', 1234)

import socket
import json
import struct
import binascii
import math
import signal
import sys
import time

class KerfConnection:
    
  def __init__(self, server, port):
    self.server = server
    self.port = port

    self.display_type = 0 #NONE:0 SHOW_MSG:1 SHOW_RESULT:2 BOTH:3

    signal.signal(signal.SIGINT, self.signal_handler)
    clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    #clientsocket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 0)
    clientsocket.connect((server, port))
    self.handle = clientsocket

    return

  def __del__(self):
    self.handle.close()
    return

  def recv_n(self, handle, wanted):
    data = ''.encode()
    while(len(data) < wanted):
      data += handle.recv(wanted - len(data))
    return data

  def async_json_send(self, string, *args):
    response_type = 0 #NONE
    return self.general_json_send(response_type, string, *args)

  def sync_json_send(self, string, *args):
    response_type = 1 #FULL
    return self.general_json_send(response_type, string, *args)
 
  def general_json_send(self, response_type, string, *args):
  
    bundle = [string, args]
    code = json.dumps(bundle)
    code_size = len(code)

    execution_type = 4  #JSON
    response_type  = response_type #NONE:0 FULL:1 PARTIAL:2
    display_type   = self.display_type
    any_size       = 16 + code_size
    m              = int(math.ceil(math.log(any_size,2)))
    wire_size      = int(2**m)
    missing        = wire_size - any_size
  
    wire_net = socket.htonl(wire_size)
  
    ref_net  = 1
    code_net = code_size #socket.htonl(code_size & 0xffffffff )
  
    pad      = missing * '\0'
  
    string = "bbbb bbbb xxxxI bbbbI Ixxxx"
    packed = struct.pack(string, 
                         0, 0, 0, 0, 
                         execution_type, response_type, display_type, 0,
                         wire_net,
                         m, 0, 0, -1, 
                         ref_net,
                         code_net)
  
    self.handle.send(packed + code.encode() + pad.encode())
  
    if (0 == response_type):
      return None

    data = self.recv_n(self.handle, 32)
  
    header = struct.unpack(string, data)
    totality = 16 + socket.ntohl(header[8] & 0xffffffff )
    size =  header[14]
  
    string = self.recv_n(self.handle, size)
  
    more = max(0, totality - (size + 32))
    if (more > 0):
      pad = self.recv_n(self.handle, more)
  
    obj = json.loads(string.decode())
  
    return obj
  
  def signal_handler(self, signal, frame):
      print('Closing socket on Ctrl+C...')
      self.handle.close()


if __name__ == '__main__':

    server = 'localhost'
    port = 1234

    kerf = KerfConnection(server, port)

    response = kerf.sync_json_send("2+3")
    print(response)
    
    response = kerf.sync_json_send("a:$1;b:$2", 333, 444) #remote assignment
    print(response)
    
    response = kerf.sync_json_send("1+1;") #muted response
    print(response)
    
    response = kerf.sync_json_send("t:{{a:1 2 3, b: 4 5 6, c: [now(), now(), now()]}}; select * from t where a = 1")
    print(response)


    start = time.time()
    
    for i in range(10000):
      response = kerf.async_json_send("1+1")
    
    end = time.time()
    print(end - start)

