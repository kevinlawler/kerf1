kerf.sync.json.send <-
function(x,b=list(),conn,unpack.table=TRUE,displaydebug=FALSE) {
    if(displaydebug=TRUE) {
        dispd=3L ## show stuff on the kerf side
    } else {
        dispd=0L ## don't show stuff on the kerf side
    }
    
    out <- kerf.gen.send(kerf.json(x,b),con=conn,distype=dispd,restype=1L)
    if(unpack.table) {
        kerf.list.framer(out)
    } else {
        out
    }
}
