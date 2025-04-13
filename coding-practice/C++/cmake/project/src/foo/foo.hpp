#ifndef FOO_HPP
#define FOO_HPP

template <typename D>
class Foo
{
private:
  D data;

protected:
  // Nothing

public:
  Foo(void);
  virtual ~Foo(void);

  void reset(void);
  D foo(void); 
};

#endif
