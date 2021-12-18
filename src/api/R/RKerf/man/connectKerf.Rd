\name{connectKerf}
\alias{connectKerf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to send commands to kerf and receive back data from kerf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
con <- connectKerf(host, port)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{host}{is a string representing the address of a kerf server.
    
  }

  \item{port}{ is a number representing the port of a kerf server.
    
  }

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  return value is a kerf connection object.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kerf software
}
\note{
See kerf.sync.json.send for details on what to do with it.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

\dontrun{
con <- connectKerf("localhost",1234)
}
}
\keyword{ kerf }
\keyword{ data }

