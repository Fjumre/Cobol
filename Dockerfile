FROM debian:latest

# Install GnuCOBOL + SQLite CLI + dev libs
RUN apt-get update && apt-get install -y \
    gnucobol \
    sqlite3 \
    libsqlite3-dev \
    build-essential \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Work directory inside container
WORKDIR /app

# Copy everything (COBOL, txt, schema.sql) into the image
COPY . /app

# By default we just drop into a shell; you can override with docker run
CMD ["/bin/bash"]
