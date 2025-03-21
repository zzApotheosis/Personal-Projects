# CORBA Example
This is a simple echo server example using CORBA in C++. This example uses

# Build and Run This Example

Build the skeleton classes from the IDL definitions. This will produce the skeleton code for C++ classes to inherit and implement.

> $ omniidl -bcxx echo.idl

In one terminal, run these commands to build and run the echo server.

> $ clang++ \`pkg-config --cflags --libs omniORB4\` -o echo-server echo-server.cc echoSK.cc  
> $ ./echo-server

In a second terminal, run these commands to build and run the client.

> $ clang++ \`pkg-config --cflags --libs omniORB4\` -o echo-client echo-client.cc echoSK.cc  
> $ ./echo-client

See that the client sends and receives a message with CORBA.

Additionally, there is an example that defines a custom message that can be passed and processed through CORBA. Build the `message` echo server with:

> $ omniidl -bcxx message.idl

In one terminal, run these commands to build and run the message-based echo server.

> $ clang++ \`pkg-config --cflags --libs omniORB4\` -o message-server message-server.cc messageSK.cc  
> $ ./message-server

In a second terminal, run these commands to build and run the message-based echo client.

> $ clang++ \`pkg-config --cflags --libs omniORB4\` -o message-client message-client.cc messageSK.cc  
> $ ./message-client

See that the client sends and receives a customized message with CORBA.
