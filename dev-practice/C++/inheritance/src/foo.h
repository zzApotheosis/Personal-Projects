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
template <typename D> class Foo
{
public:
  Foo (void);
  virtual ~Foo (void);
  virtual void do_something (void) = 0;
  virtual void set_data(D const &);

protected:

private:
  D data;
};
#endif

#endif
