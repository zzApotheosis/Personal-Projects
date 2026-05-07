#ifndef BAR_H
#define BAR_H

#include "foo.h"

#ifdef __cplusplus
template <typename D> class Bar : public Foo<D>
{
public:
  Bar (void);
  virtual ~Bar (void);
  void do_something (void) override;
  virtual void set_more_data(const D);

protected:
private:
  D more_data;
};
#else
#error "THIS IS NOT SUPPORTED IN C"
#endif

#endif
