#!/bin/sh
./backup
#curl --remote-name https://ci.opencollab.dev/job/GeyserMC/job/Geyser/job/master/lastSuccessfulBuild/artifact/bootstrap/standalone/target/Geyser.jar
firejail /usr/bin/java -jar server.jar nogui
