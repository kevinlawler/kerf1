var net = require('net');

var data = new Buffer(0)
var data_expected = Number.MAX_SAFE_INTEGER;

function kerf_client_query(host, port, query, args, callback) {
  var client = _kerf_client_create(host, port, callback)
  _kerf_client_send(client, query, args)
}

function _kerf_client_create(host, port, callback) {

  var client = new net.Socket();

  client.on('close', function() {
    //console.log('Connection closed');
    data = new Buffer(0)
    data_expected = Number.MAX_SAFE_INTEGER;
  });
  
  client.on('data', function(chunk) {
    data += chunk;

    //console.log('Received: ' + chunk + ' | Data length: ' + data.length);

    if(data.length >= 32) {
      //for(var i = 0; i < data.length; i++) {
      // console.log('data['+i+']: ' + data[i].charCodeAt());
      //}

      var buf = new Buffer([data[24].charCodeAt(),data[25].charCodeAt(),data[26].charCodeAt(),data[27].charCodeAt()])
      var count = buf.readInt32LE();

      data_needed = 32 + count; 
    }

    //we're just reading the bytes we need and then closing the socket
    //this is a bit fast and loose, but should be OK for now
    if(data.length >= data_needed) {
      //console.log("On completed read")

      var json = data.slice(32, 32+count)

      object = JSON.parse(json)

      callback(object);
      client.destroy();
    }

  });
 
  client.connect(port, host, function() {
    //console.log('Connected');
  });

  return client;
}

function _kerf_repeat(string, count) {
    if (count < 1) return '';
    var result = '', pattern = string.valueOf();
    while (count > 1) {
        if (count & 1) result += pattern;
        count >>= 1, pattern += pattern;
    }
    return result + pattern;
};

function _kerf_client_send(client, query, args) {

  if(!Array.isArray(args)) {
    args = [args]
  }

  var bundle = [query, args]

  code = JSON.stringify(bundle)

  code_size = code.length

  execution_type = 4  //JSON
  response_type  = 1  //NONE:0 FULL:1 PARTIAL:2
  display_type   = 3  //NONE:0 SHOW_MSG:1 SHOW_RESULT:2 BOTH:3
  any_size       = 16 + code_size

  m              = Math.ceil(Math.log(any_size)/ Math.log(2))
  wire_size      = Math.floor(Math.pow(2,m))
  missing        = wire_size - any_size

  ref_net  = 1
  code_size

  pad      = _kerf_repeat("\0", missing)

  const wireb = Buffer.allocUnsafe(4);
  wireb.writeInt32BE(wire_size, 0);

  const refb = Buffer.allocUnsafe(4);
  refb.writeInt32LE(ref_net, 0);

  const codeb = Buffer.allocUnsafe(4);
  codeb.writeInt32LE(code_size, 0);

  buff = new Buffer([0, 0, 0, 0, 
                     execution_type, response_type, display_type, 0,
                     0, 0, 0, 0, wireb[0], wireb[1], wireb[2], wireb[3], 
                     m, 0, 0, -1, refb[0], refb[1], refb[2], refb[3], 
                     codeb[0], codeb[1], codeb[2], codeb[3], 0, 0, 0, 0]);

  client.write(buff)
  client.write(code)
  client.write(pad)
}

function test() {

  var host  = '127.0.0.1'
  var port  = 1234
  var query = 'a:$1+$2'
  var query2 = 't:{{a:1 2 3, b: 4 5 6, c: [now(), now(), now()]}}; select * from t where a = 1'
  var query3 = '10000 repeat 3'
  var args  = [3,4]
  var callback = function(object) {
    console.log("Callback result: " + object);
  }

  kerf_client_query(host, port, query, args, callback);
}

test()

