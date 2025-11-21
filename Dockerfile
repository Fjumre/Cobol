FROM debian:latest

# Install tools needed for building OCESQL
RUN apt-get update && apt-get install -y \
    gnucobol \
    sqlite3 \
    build-essential \
    autoconf \
    automake \
    libtool \
    git \
    && apt-get clean

# Download and build OCESQL (COBOL + SQLite precompiler)
RUN git clone https://github.com/opensourcecobol/opensource-cobol-esql.git /tmp/ocesql && \
    cd /tmp/ocesql && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install

# Create folder for COBOL + database files
WORKDIR /app

# Default command (you will override this when running)
CMD ["/bin/bash"]
