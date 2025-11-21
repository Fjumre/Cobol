FROM debian:latest

# Install dependencies
RUN apt-get update && apt-get install -y \
    gnucobol \
    sqlite3 \
    libcob-sql \
    ocesql \
    build-essential \
    && apt-get clean

WORKDIR /app

CMD ["/bin/bash"]
