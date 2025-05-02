#include <climits>
#include <grpcpp/grpcpp.h>
#include <iostream>
#include <memory>
#include <string>

#include "echo.grpc.pb.h"

using namespace std;

class EchoServiceImpl : public EchoService::Service
{
public:
  grpc::Status echo(grpc::ServerContext * const ctx, const EchoRequest * const req, EchoResponse * const res)
  {
    if (n == ULONG_LONG_MAX)
      n = 0; // No buffer overflows allowed
    
    string msg = req->msg();
    msg += ' ';
    msg += to_string(n++);
    cerr << "Responding to 'echo' request with message: '" << msg << "'\n";
    res->set_msg(std::move(msg));
    
    return grpc::Status::OK;
  }

private:
  unsigned long long int n = 0;
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
  std::unique_ptr<grpc::Server> server = builder.BuildAndStart();
  if (!server) {
    cerr << "failed to create server.\n";
    return 1;
  }

  cerr << "Started server on: " << argv[1] << '\n';
  server->Wait();
}
