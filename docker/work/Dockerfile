FROM ubuntu:18.04

SHELL ["/bin/bash", "-c"]

RUN apt-get update \
    && apt-get install -y locales \
    && locale-gen ja_JP.UTF-8

RUN apt-get install -y \
      git \
      vim \
      curl \
      wget

WORKDIR /root

RUN git clone https://github.com/syala-hasu/dotfiles.git

WORKDIR /root/dotfiles
RUN ./init.sh

WORKDIR /root/workspace
