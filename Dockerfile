FROM node:8.11.2-jessie

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils && apt-get install -y libzmq3-dev && apt-get install pkg-config

VOLUME /meccg

WORKDIR /meccg

ENV DEBIAN_FRONTEND teletype
