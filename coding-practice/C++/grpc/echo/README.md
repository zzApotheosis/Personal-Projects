# gRPC C++ Echo Server Example

## Building and running this example

> $ protoc --cpp_out=. --grpc_out=. --plugin=protoc-gen-grpc=\`which grpc_cpp_plugin\` echo.proto  
> $ clang++ \`pkg-config --cflags --libs grpc++ protobuf\` echo-server.cc echo.pb.cc echo.grpc.pb.cc -o echo-server  
> $ clang++ \`pkg-config --cflags --libs grpc++ protobuf\` echo-client.cc echo.pb.cc echo.grpc.pb.cc -o echo-client  
> $ ./echo-server

Open a second terminal and run this:

> $ ./echo-client
