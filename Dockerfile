FROM node:6.4
MAINTAINER Guilherme Henrique <gjhenrique@gmail.com>

RUN apt-get update -qq -y
RUN apt-get install texlive-full -y
