import socket
import json
import struct
import binascii
import math
import signal
import sys

def recv_n(handle, wanted):
  data = ''
  while(len(data) < wanted):
    data += handle.recv(wanted - len(data))
  return data

def kerf_sync_json_send(handle, string, *args):

  bundle = [string, args]
  code = json.dumps(bundle)
  code_size = len(code)

  execution_type = 4  #JSON
  response_type  = 1  #NONE:0 FULL:1 PARTIAL:2
  display_type   = 3  #NONE:0 SHOW_MSG:1 SHOW_RESULT:2 BOTH:3
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

  #OPTIMIZATION_POINT MSG_MORE
  handle.send(packed)
  handle.send(code)
  handle.send(pad)

  data = recv_n(handle, 32)

  header = struct.unpack(string, data)
  totality = 16 + socket.ntohl(header[8] & 0xffffffff )
  size =  header[14]

  string = recv_n(handle, size)

  more = max(0, totality - (size + 32))
  if (more > 0):
    pad = recv_n(handle, more)

  obj = json.loads(string)

  return obj

def kerf_signal_handler(signal, frame):
  print('Closing socket on Ctrl+C...')
  clientsocket.close()

server = 'localhost'
port = 1234

signal.signal(signal.SIGINT, kerf_signal_handler)

clientsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
clientsocket.connect((server, port))

response = kerf_sync_json_send(clientsocket, "2+3")
print response

response = kerf_sync_json_send(clientsocket, "a:$1;b:$2", 333, 444) #remote assignment
print response

response = kerf_sync_json_send(clientsocket, "1+1;") #muted response
print response

response = kerf_sync_json_send(clientsocket, "t:{{a:1 2 3, b: 4 5 6, c: [now(), now(), now()]}}; select * from t where a = 1")
print response

clientsocket.close()

