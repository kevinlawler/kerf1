FROM alpine:3.16 AS build
RUN apk update && apk add clang musl-dev compiler-rt lld binutils make ncurses-dev fts-dev zlib-dev zlib-static libedit-dev libedit-static
RUN cd /tmp/
RUN wget http://ftp.gnu.org/gnu/termcap/termcap-1.3.1.tar.gz
RUN echo "91a0e22e5387ca4467b5bcb18edf1c51b930262fd466d5fda396dd9d26719100  termcap-1.3.1.tar.gz" | sha256sum -c
RUN tar xf termcap-1.3.1.tar.gz && cd termcap-1.3.1
RUN ln -s /usr/bin/clang /usr/bin/cc
RUN ./configure && make && make install
RUN mkdir /tmp/kerf1 && cd /tmp/kerf1/
ADD ./* /tmp/kerf1/
RUN cd src
RUN STATIC=1 make all

FROM scratch AS export-release
COPY --from=build /tmp/kerf1/src/kerf /

FROM scratch AS export-test
COPY --from=build /tmp/kerf1/src/kerf_test /
