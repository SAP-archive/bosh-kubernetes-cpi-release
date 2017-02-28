FROM scratch

ADD  build/image.tgz /

RUN apt-get -y install libgmp3-dev

COPY ./agent.json /var/vcap/bosh/agent.json
