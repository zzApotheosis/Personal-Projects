#ifndef FOO_H
#define FOO_H

#ifdef __cplusplus
extern "C" {
#endif
  void library_function(const int);
  int echo_server(const int, char * []);
  int echo_client(const int, char * []);
#ifdef __cplusplus
}
#endif

#endif
