FROM ubuntu:18.04

WORKDIR /srv

RUN apt-get update && apt-get -qq -y install sassc

ADD dist /srv/dist
ADD .stack-work/install/x86_64-linux/lts-12.5/8.4.3/bin/storage-costs-evaluator /srv/storage-costs-evaluator

# Styles rebuilding
# Dependencies needed to rebuild styles
COPY node_modules/bootstrap web/~bootstrap
COPY node_modules/bootswatch web/~bootswatch
COPY node_modules/@fortawesome web/~fortawesome
COPY web/scss /web/scss
RUN ./bin/customize.sh

CMD ["./storage-costs-evaluator"]
