FROM hseeberger/scala-sbt:11.0.3_1.2.8_2.13.0 as builder
WORKDIR /build
COPY . .
RUN sbt clean dist
RUN mkdir starchat_build
RUN unzip -d starchat_build target/universal/starchat*.zip

FROM openjdk:12
COPY --from=builder /build/starchat_build/starchat* /starchat
EXPOSE 8888 8443
CMD ["/starchat/scripts/utils/wait-for-it.sh", "getjenny-es:9200", "-t", "15", "--", "/starchat/bin/starchat"]