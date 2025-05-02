#include <stdint.h>
#include <stdio.h>
#include <limits.h>

#define BITS_PER_GROUP 4
#define BITS_PER_BYTE 8

typedef uint32_t T;
typedef uint64_t ITER;

void print_bit_group(FILE * const fp, const T n)
{
  uint8_t bit = 0;
  T bit_index = 1;
  for (ITER i = 0; i < BITS_PER_GROUP; i++)
    {
      bit_index <<= 1;
    }
  for (ITER i = 0; i < BITS_PER_GROUP; i++)
    {
      bit_index >>= 1;
      bit = n & bit_index;
      if (bit > 0)
	bit = 1;
      fprintf(fp, "%u", bit);
    }
}

int main(void)
{
  T nums[] = {
    70,
    36774,
    24,
    0,
    123,
    933444,
    2,
    1,
    UINT32_MAX,
    0b10101010101010100101010101010101,
  };
  const ITER num_bit_groups = sizeof(T) * BITS_PER_BYTE / BITS_PER_GROUP;
  T num = 0;

  /* Iterate over each num */
  for (ITER i = 0; i < sizeof(nums) / sizeof(T); i++)
    {
      /* Announce which number we're processing */
      fprintf(stderr, "num = %u\n", nums[i]);

      /* Iterate over each bit group */
      for (ITER j = 0; j < num_bit_groups; j++)
	{
	  /* Capture the original number */
	  num = nums[i];
	  
	  /* Bit-shift by k number of bit groups */
	  for (ITER k = j + 1; k < num_bit_groups; k++)
	    {
	      num >>= BITS_PER_GROUP;
	    }

	  /* Show what the bits are */
	  print_bit_group(stderr, num);
	  fprintf(stderr, "\n");
	}
    }
}
