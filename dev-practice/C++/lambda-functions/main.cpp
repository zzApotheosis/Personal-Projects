#include <iostream>
#include <vector>
#include <functional>

int main(void)
{
  std::vector<std::function<void (void)>> function_vector;
  function_vector.push_back([]() -> void {
    std::cerr << "Is this the Krusty Krab?" << std::endl;
  });
  function_vector.push_back([]() -> void {
    std::cerr << "No, this is Patrick! >:(" << std::endl;
  });

  for (auto f : function_vector)
    {
      f();
    }
  
  return 0;
}
