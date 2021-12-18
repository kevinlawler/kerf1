kerf.json <-
function(x,b=list()) {
    cx=class(x)
    cb=class(b)
    if(cx=="character") {
        switch(cb,
               list = as.character(jsonlite:::toJSON(list(x,b),POSIXt="ISO8601",auto_unbox=TRUE,dataframe="columns")),
               numeric = as.character(jsonlite:::toJSON(list(x,list(b)),POSIXt="ISO8601",auto_unbox=TRUE,dataframe="columns")),
               data.frame = as.character(jsonlite:::toJSON(list(x,list(b)),POSIXt="ISO8601",auto_unbox=TRUE,dataframe="columns")),
               warning("unsupported type"))
    } else {
        warning("argument should be string to send to kerf")
    }
}
