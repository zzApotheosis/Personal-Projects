#include <grpcpp/grpcpp.h>
#include <iostream>
#include <unistd.h>

#include "echo.grpc.pb.h"

using namespace std;

int main(int argc, char **argv)
{
  if (argc != 2)
    {
      cerr << "Usage: " << argv[0] << " <server address>";
      return 1;
    }

  std::shared_ptr<grpc::Channel> channel = grpc::CreateChannel(argv[1], grpc::InsecureChannelCredentials());
  std::unique_ptr<EchoService::Stub> stub = EchoService::NewStub(channel);

  for (;;) {
    grpc::ClientContext ctx;
    EchoResponse resp;
    EchoRequest req;
    req.set_msg("Hello");
    grpc::Status status = stub->echo(&ctx, req, &resp);

    if (status.ok()) {
      cerr << "Response message: " << resp.msg() << '\n';
    } else {
      cerr << "Error: " << status.error_message() << ": " << status.error_details() << '\n';
    }

    sleep(1);
  }
}
