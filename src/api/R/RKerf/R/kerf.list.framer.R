kerf.list.framer <-
function(x) {
    out = x
    if(class(x)=="list") {
        keys = names(x)
        isjson = grep("is_json_table",keys)
        if(length(isjson)==1){
            out= data.frame(x[keys[-1*isjson]])
        }
    }
    out
}
