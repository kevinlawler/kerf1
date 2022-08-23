FROM alpine:3.16 AS build
RUN apk update && apk add clang musl-dev compiler-rt lld binutils make ncurses-dev fts-dev zlib-dev zlib-static libedit-dev libedit-static
RUN wget http://ftp.gnu.org/gnu/termcap/termcap-1.3.1.tar.gz
RUN echo "91a0e22e5387ca4467b5bcb18edf1c51b930262fd466d5fda396dd9d26719100  termcap-1.3.1.tar.gz" | sha256sum -c
RUN tar xf termcap-1.3.1.tar.gz
RUN ln -s /usr/bin/clang /usr/bin/cc
RUN cd termcap-1.3.1 && ./configure && make && make install
RUN mkdir /kerf1/
ADD ./* /kerf1/
RUN cd /kerf1/ && STATIC=1 make all
RUN tar cvf kerf1.tar -C /kerf1/ kerf kerf_test

FROM scratch AS export
COPY --from=build /kerf1.tar /
