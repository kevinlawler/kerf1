the R api can be installed at the command like with
R CMD INSTALL RKerf_0.1.0.tar.gz

It depends on the packages pack and jsonlite, so these need to be installed first, either at command line or with
install.packages(c("pack","jsonlite"))

for internal builds, modify the code in the directory, then build a new one with
R CMD build RKerf

