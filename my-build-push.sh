#!/bin/bash

export DOCKER_DEFAULT_PLATFORM=linux/amd64
docker build -f docker/Dockerfile . -t nodje/geneweb:latest
docker push nodje/geneweb
