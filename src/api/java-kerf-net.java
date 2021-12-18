//SERVER
//    Run: ./kerf -p 1234
//CLIENT
//Compile: javac -cp ".:gson.jar" java-kerf-net.java
//    Run: java  -cp ".:gson.jar" KerfNetJavaAPI
//
//  Google's Gson library is required (https://github.com/google/gson/blob/master/LICENSE)


//Example
//  synchronousRemoteExecute(socket, "v:$1+$2", 3, 4);
//Add the numbers 3 and 4 together on the server and store it in v on the server.
//Return the created value (7) to the client. (Use semicolon at the end to suppress
//returning long values.) 

import java.io.InputStream;;
import java.io.OutputStream;
import java.io.DataOutputStream;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Random;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

class KerfNetJavaAPI {

  boolean SHOW_MESSAGES = false;

  public static byte[] intToBytes(int i) {
    ByteBuffer buffer = ByteBuffer.allocate(4);
    buffer.putInt(i);
    return buffer.array();
  }
 
  public static byte[] longToBytes(long l) {
    ByteBuffer buffer = ByteBuffer.allocate(8);
    buffer.putLong(l);
    return buffer.array();
  }
  
  public static long bytesToLong(byte[] b) {
    ByteBuffer buffer = ByteBuffer.allocate(8);
    buffer.put(b);
    buffer.flip();
    return buffer.getLong();
  }

  byte[] headerNoSize() {

    byte[] b = new byte[16];

    byte execution_type = 4; //JSON
    byte response_type  = 1; //NONE:0 FULL:1 PARTIAL:2
    byte display_type   = 0; //NONE:0 SHOW_MSG:1 SHOW_RESULT:2 BOTH:3

    if(SHOW_MESSAGES) {
      display_type = 3;
    }

    b[0] = 0;
    b[1] = 0;
    b[2] = 0;
    b[3] = 0;

    b[4] = execution_type; 
    b[5] = response_type; 
    b[6] = display_type;
    b[7] = 0;

    b[8]  = 0;
    //...
    b[15] = 0;
    
    return b; 
  }

  byte[] headerForSize(long n) {

    byte[] b = headerNoSize();

    byte[] c = longToBytes(n);

    System.arraycopy(c, 0, b, 8, c.length);

    return b;
  }

  byte[] kerfHeaderForCharvec(long n) {

    long strlen   = n;
    long any_size = 16 + strlen;
    byte m        = (byte)Math.ceil(Math.log(any_size)/Math.log(2));

    byte[] b = new byte[16];

    b[0] =  m;
    b[1] =  0;
    b[2] =  0;
    b[3] = -1;

    byte[] r = intToBytes(-1);
    System.arraycopy(r, 0, b, 4, r.length);

    byte[] c = longToBytes(strlen);

    reverseBytes(c);

    System.arraycopy(c, 0, b, 8, c.length);

    return b; 
  }

  void reverseBytes(byte [] b) {
    int i =0;

    int n = b.length;

    //reverse
    for(i=0; i<n/2; i++) 
    {
      byte t = b[i];
      b[i] = b[n-i-1];
      b[n-i-1]=t;
    }
  }

  void sendJsonBundle(Socket socket, String json) throws Exception {

    long strlen    = json.length();
    long any_size  = 16 + strlen;
    byte m         = (byte)Math.ceil(Math.log(any_size)/Math.log(2));

    long wire_size = (long)Math.pow(2, m);
    long missing   = wire_size - any_size;

    OutputStream outputStream = socket.getOutputStream(); 
    DataOutputStream dos = new DataOutputStream(outputStream);

    byte[] message_header = headerForSize(wire_size);
    dos.write(message_header, 0, message_header.length);

    byte[] kerf_header = kerfHeaderForCharvec(strlen);
    dos.write(kerf_header, 0, kerf_header.length);

    byte[] bundle_bytes = json.getBytes();
    dos.write(bundle_bytes, 0, bundle_bytes.length);

    byte[] pad = new byte[(int)missing];  
    dos.write(pad, 0, pad.length);
  }

  String recvJson(Socket socket) throws Exception {

    InputStream in = socket.getInputStream();

    int pre = 8;
    byte[] preb = new byte[pre];
    in.read(preb, 0, pre);

    int middle = 8;
    byte[] middleb = new byte[middle];
    in.read(middleb, 0, middle);
    long incoming = bytesToLong(middleb);

    int post = 8;
    byte[] postb = new byte[post];
    in.read(postb, 0, post);

    int second = 8;
    byte[] c = new byte[second];
    in.read(c, 0, second);
    reverseBytes(c);

    long strlen = bytesToLong(c);

    long any_size  = 16 + strlen;
    byte m         = (byte)Math.ceil(Math.log(any_size)/Math.log(2));
    long wire_size = (long)Math.pow(2, m);

    byte[] d = new byte[(int)strlen];
    in.read(d, 0, (int)strlen);

    String json = new String(d);

    int i =0;

    long more = incoming - (16 + strlen);

    for(i = 0; i < more; i++) in.read();

    return json;
  }

  Object synchronousRemoteExecute(Socket socket, String query, Object... args) throws Exception {

    Object [] bundle = {query, args};

    Gson gson = new GsonBuilder().create();

    String json_send = gson.toJson(bundle);

    long n = json_send.length();

    sendJsonBundle(socket, json_send);

    String json_recv = recvJson(socket);

    Object response = gson.fromJson(json_recv, Object.class);

    return response;
  }

  void runExample() throws Exception {
    String host = "127.0.0.1";
    int port = 1234;

    Socket socket = new Socket(host, port);

    Object response = null;

    int i = 0;
    int n = 10000;

    for(i = 0; i < n; i++) {
    }

    SHOW_MESSAGES = true;

    response = synchronousRemoteExecute(socket, "1+1");
    //response = synchronousRemoteExecute(socket, "a:11+22");
    //response = synchronousRemoteExecute(socket, "a:$1+$2", 3, 4);
    //response = synchronousRemoteExecute(socket, "{[x,y] x/y }($1,$2)", 10, 100);

    if(SHOW_MESSAGES) {
      System.out.println("Response: " + response);
    }
  }

  public static void main(String[] args) throws Exception {
    KerfNetJavaAPI api = new KerfNetJavaAPI();
    api.runExample();
  }

}

