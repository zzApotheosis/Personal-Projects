#include <grpcpp/grpcpp.h>
#include <iostream>
#include <string>

#include "echo.grpc.pb.h"

using namespace std;

class EchoServiceImpl : public EchoService::Service
{
  grpc::Status Echo(grpc::ServerContext *ctx, const EchoRequest *req, EchoResponse *res)
  {
    string msg = req->msg();
    msg += ' ';
    msg += to_string(n++);
    cerr << "Responding to 'echo' request with message: '" << msg << "'\n";
    res->set_msg(std::move(msg));
    return grpc::Status::OK;
  }

  int n = 0;
};

int main(int argc, char **argv)
{
  if (argc != 2) {
    cout << "Usage: " << argv[0] << " <listen address>";
    return 1;
  }

  EchoServiceImpl service;

  grpc::ServerBuilder builder;
  builder.AddListeningPort(argv[1], grpc::InsecureServerCredentials());
  builder.RegisterService(&service);
  auto server = builder.BuildAndStart();
  if (!server) {
    cerr << "failed to create server.\n";
    return 1;
  }

  cerr << "Started server on: " << argv[1] << '\n';
  server->Wait();
}
