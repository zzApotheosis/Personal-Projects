#ifndef FOO_H
#define FOO_H

#ifdef __cplusplus
extern "C"
{
#endif
  void library_function (const int);
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
class Foo
{
public:
  Foo (void);
  ~Foo (void);
  void do_something (void);

protected:

private:
};
#endif

#endif
