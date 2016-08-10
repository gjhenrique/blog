FROM mwallasch/docker-ruby-node
MAINTAINER Guilherme Henrique <gjhenrique@gmail.com>

RUN apt-get update -qq -y
RUN apt-get install texlive-full -y
