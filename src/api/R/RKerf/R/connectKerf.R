connectKerf <-
    function(addr,port,blocking=TRUE) {
    socketConnection(host=addr,port=port,open="r+b",blocking=blocking) 
}
