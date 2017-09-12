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
     jq \
     vim \
	&& apt-get clean
