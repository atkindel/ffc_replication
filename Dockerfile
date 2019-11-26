FROM rocker/r-ver:3.5.1

ARG ffc_repo_version=0dd6d22

# Install git
RUN apt-get update && apt-get upgrade -y && apt-get install -y git

# Install R package system dependencies
RUN apt-get install -y curl
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libxml2-dev
RUN apt-get install -y wget
RUN apt-get install -y libz-dev

# Clone replication code and data repository
RUN cd /home/
RUN git clone https://github.com/atkindel/ffc_replication.git && cd ffc_replication && git checkout $ffc_repo_version
