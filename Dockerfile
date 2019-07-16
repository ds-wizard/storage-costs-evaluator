FROM ubuntu:18.04

WORKDIR /srv

ADD dist /srv/dist
ADD .stack-work/install/x86_64-linux/lts-12.5/8.4.3/bin/storage-costs-evaluator /srv/storage-costs-evaluator

CMD ["./storage-costs-evaluator"]
