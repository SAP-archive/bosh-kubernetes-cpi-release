FROM loewenstein/bosh-stemcell-warden-boshlite-ubuntu-trusty-go_agent

RUN apt-get -y install libgmp3-dev

COPY ./agent.json /var/vcap/bosh/agent.json
