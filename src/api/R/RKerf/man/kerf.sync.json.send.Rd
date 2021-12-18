\name{kerf.sync.json.send}
\alias{kerf.sync.json.send}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to send commands to kerf and receive back data from kerf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kerf.sync.json.send(x, b = list(), conn, unpack.table = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{is a string representing a valid kerf command; you must
    string escape any quotes in the kerf command.
  }

  \item{b}{ is a list representing data you want to send  to kerf
  }
  \item{conn}{
       is the connector opened by connectKerf
}

  \item{unpack.table}{
    If \code{unpack.table} is true, returned tables will
    be unpacked into a data frame
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  return values are either a null, a vector, a list or a data frame
  depending on returned values from kerf server. Kerf maps are returned
  as lists, Kerf vectors and atoms as R vectors, tables will either be
  returned as lists or data.frames depending on truthiness of unpack.table.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kerf software
}
\note{
See reference manual for json details
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

\dontrun{
con <- connectKerf("localhost",1234)
kerf.sync.json.send("1+1",conn=con)
[1] 2
kerf.sync.json.send("1+1;",conn=con)
NULL
## send some data
  kerf.sync.json.send("a:$1;",list(rnorm(10)),conn=con)
## send more data into a two column table
  kerf.sync.json.send("tbl:{{a:$1,b:$2}};",list(1:10,11:20),conn=con)
NULL
## query the table
  kerf.sync.json.send("select * from tbl where a < 3",conn=con)
  a  b
1 1 11
2 2 12
}
}
\keyword{ kerf }
\keyword{ data }

