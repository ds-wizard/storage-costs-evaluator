FROM ubuntu:18.04

WORKDIR /srv

RUN apt-get update && apt-get -qq -y install sassc

COPY dist dist
COPY ./storage-costs-evaluator storage-costs-evaluator

# Styles rebuilding
# Dependencies needed to rebuild styles
COPY node_modules/bootstrap web/~bootstrap
COPY node_modules/bootswatch web/~bootswatch
COPY node_modules/@fortawesome web/~fortawesome
COPY web/scss web/scss
COPY bin bin
RUN ./bin/customize.sh

CMD ["./storage-costs-evaluator"]
