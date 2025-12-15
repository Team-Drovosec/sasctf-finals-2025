FROM gradle:8.10.0-jdk21 AS plugin-builder
WORKDIR /build/GTARP
COPY ./GTARP/ ./
RUN chmod +x gradlew && ./gradlew --no-daemon build

FROM openjdk:21
COPY ./server/ /server
COPY --from=plugin-builder /build/GTARP/build/libs/GTARP-*.jar /server/plugins/
WORKDIR /server

CMD ["java", "-Xms6000M", "-Xmx6000M", "-XX:+AlwaysPreTouch", "-XX:+DisableExplicitGC", "-XX:+ParallelRefProcEnabled", "-XX:+PerfDisableSharedMem", "-XX:+UnlockExperimentalVMOptions", "-XX:+UseG1GC", "-XX:G1HeapRegionSize=8M", "-XX:G1HeapWastePercent=5", "-XX:G1MaxNewSizePercent=40", "-XX:G1MixedGCCountTarget=4", "-XX:G1MixedGCLiveThresholdPercent=90", "-XX:G1NewSizePercent=30", "-XX:G1RSetUpdatingPauseTimePercent=5", "-XX:G1ReservePercent=20", "-XX:InitiatingHeapOccupancyPercent=15", "-XX:MaxGCPauseMillis=200", "-XX:MaxTenuringThreshold=1", "-XX:SurvivorRatio=32", "-Dminecraft.api.env=custom", "-Dminecraft.api.auth.host=http://auth-wiedersehen:25566/authserver", "-Dminecraft.api.account.host=http://auth-wiedersehen:25566/accountserver", "-Dminecraft.api.profiles.host=http://auth-wiedersehen:25566/accountserver", "-Dminecraft.api.session.host=http://auth-wiedersehen:25566/sessionserver", "-Dminecraft.api.services.host=http://auth-wiedersehen:25566/servicesserver", "-jar", "paper.jar", "nogui"]
