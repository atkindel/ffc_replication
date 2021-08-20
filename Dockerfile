# This Dockerfile was used in March 2020 to create Docker v11
# All subsequent Docker versions are derived directly from the v11 image
# We recommend using the images on Docker Hub rather than rebuilding the image from scratch

# FROM rocker/r-ver:3.5.1
# 
# ARG ffc_repo_version=85ce94e  # NOTE: v11 commit ID
# 
# # Install git
# RUN apt-get update && apt-get upgrade -y && apt-get install -y git
# 
# # Install R package system dependencies
# RUN apt-get install -y curl
# RUN apt-get install -y libcurl4-openssl-dev
# RUN apt-get install -y libssl-dev
# RUN apt-get install -y libxml2-dev
# RUN apt-get install -y wget
# RUN apt-get install -y libz-dev
# 
# # Clone replication code and data repository
# RUN cd /home/
# RUN git clone https://github.com/atkindel/ffc_replication.git && cd ffc_replication && git checkout $ffc_repo_version
