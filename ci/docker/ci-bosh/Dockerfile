FROM loewenstein/ci-gcloud

# install requirements for bosh create-env
RUN sudo apt-get install -y \
  zlibc \
  zlib1g-dev \
  openssl \
  libxslt-dev \
  libxml2-dev \
  libssl-dev \
  libreadline6 \
  libreadline6-dev \
  libyaml-dev \
  libsqlite3-dev \
  sqlite3 \
  && apt-get clean

# install requirements for gmp
RUN sudo apt-get install -y \
  m4 \
  && apt-get clean

ADD install-ruby.sh /tmp/install-ruby.sh
RUN /tmp/install-ruby.sh

# bosh cli
RUN    curl -o /usr/local/bin/bosh https://s3.amazonaws.com/bosh-cli-artifacts/bosh-cli-2.0.28-linux-amd64 \
    && chmod +x /usr/local/bin/bosh

# rescue go cli
RUN cp /usr/local/bin/bosh /usr/local/bin/bosh2