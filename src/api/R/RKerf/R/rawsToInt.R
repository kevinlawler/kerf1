rawsToInt <-
function(x) {
 sum(as.integer(x) * c(1L, 256L, 65536L, 16777216L) )
}
