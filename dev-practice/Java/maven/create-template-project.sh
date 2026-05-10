#!/bin/sh

_project_dir='my-app'

rm -rf "${_project_dir}"

mvn archetype:generate -DgroupId=com.example.app -DartifactId="${_project_dir}" -DarchetypeArtifactId=maven-archetype-quickstart -DarchetypeVersion=1.5 -DinteractiveMode=false

cat > "${_project_dir}"/build.sh << 'EOF'
#!/bin/sh
mvn clean compile package
EOF
chmod 700 "${_project_dir}"/build.sh

cat > "${_project_dir}"/run.sh << 'EOF'
#!/bin/sh
java -jar target/my-app-1.0-SNAPSHOT-jar-with-dependencies.jar
EOF
chmod 700 "${_project_dir}"/run.sh
