kerf.gen.send <-
function(code,conn=con,distype=0L,restype=1L) {
    code.size = nchar(code)
    ex.type = 4L #JSON
    res.type = restype #NONE:0 FULL:1 PARTIAL:2
    dis.type = distype  #NONE:0 SHOW_MSG:1 SHOW_RESULT:2 BOTH:3
    any.size = as.integer(16L + code.size)
    m = as.integer(ceiling(log(any.size,2)))
    wire.size = as.integer(2^m)
    miss = as.integer(wire.size - any.size)
    wire.net = as.integer(wire.size)
    code.net = code.size
    rawnull=as.raw(0)
    c(rawNulls(4),as.raw(ex.type),as.raw(res.type),as.raw(dis.type),rawnull,
      rawNulls(4),rev(numToRaw(wire.size,4)),
      as.raw(m),rawNulls(2),as.raw(255),as.raw(1),rawNulls(3),
      numToRaw(code.size,4),rawNulls(4)) -> header
    writeBin(header,conn)
    writeBin(pack("a*",code),conn)
    writeBin(rawNulls(miss),conn)
    out = readBin(conn,raw(),n=32L)
    wires = rawsToInt(rev(out[13:16]))
    shard = out[17]
    wires2 = as.integer(2^as.integer(shard))
    payl = rawsToInt(out[25:28])
    totality = wires + 16L
    more = max(0,(totality - (payl + 32L)))
    if(payl>0) {
        allout = readBin(conn,raw(),n=payl)
    }
    if(more>0) {
        junk = readBin(conn,raw(),n=more)
    }
    token = paste("a",payl,sep="")
    jsonlite:::fromJSON(unpack(token,allout)[[1]]) ## needs changes for async
}
