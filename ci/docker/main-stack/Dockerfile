FROM ubuntu:14.04

RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

#basic deps
RUN apt-get update
RUN apt-get -y upgrade && apt-get clean
RUN apt-get install -y \
     build-essential \
     git \
     curl \
     wget \
     tar \
     libssl-dev \
     libreadline-dev \
     dnsutils \
     xvfb \
	&& apt-get clean

# stack dependencies
RUN apt-get install -y \
     g++ \
     gcc \
     libc6-dev \
     libffi-dev \
     libgmp-dev \
     make \
     xz-utils \
     zlib1g-dev \
     git \
     gnupg \
	&& apt-get clean

# stack
RUN    mkdir -p stack \
    && wget -O - https://www.stackage.org/stack/linux-x86_64-static \
    |  tar -C stack -zx \
    && chmod +x stack/stack-*/stack \
    && mv stack/stack-*/stack /usr/local/bin/stack

# ARG stack_lts
# RUN stack setup --resolver $stack_lts

# bosh cli
# RUN    curl -o /usr/local/bin/bosh https://s3.amazonaws.com/bosh-cli-artifacts/bosh-cli-2.0.2-linux-amd64 \
#     && chmod +x /usr/local/bin/bosh
