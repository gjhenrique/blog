FROM mwallasch/docker-ruby-node
MAINTAINER Guilherme Henrique <gjhenrique@gmail.com>

RUN apt-get update && apt-get install -y emacs24-nox && apt-get install -y graphviz
