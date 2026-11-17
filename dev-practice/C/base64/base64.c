#include <stddef.h>
#include <stdlib.h>

static const char b64_encode_table[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static const char b64_decode_table[256] = {
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
  52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
  -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
  15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
  -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
  41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
};

char * base64_encode(const char * const src, const size_t input_length)
{
  if (src == NULL)
    return NULL;
  const size_t output_length = 4 * ((input_length + 2) / 3);
  char * const result = malloc(output_length + 1);
  if (result == NULL)
    abort();

  for (size_t i = 0, j = 0; i < input_length;)
    {
      const int octet_a = i < input_length ? src[i++] : 0;
      const int octet_b = i < input_length ? src[i++] : 0;
      const int octet_c = i < input_length ? src[i++] : 0;

      const int triple = (octet_a << 16) | (octet_b << 8) | octet_c;

      result[j++] = b64_encode_table[(triple >> 18) & 0x3F];
      result[j++] = b64_encode_table[(triple >> 12) & 0x3F];
      result[j++] = b64_encode_table[(triple >> 6)  & 0x3F];
      result[j++] = b64_encode_table[triple         & 0x3F];
    }

  const size_t mod = input_length % 3;
  if (mod > 0)
    {
      result[output_length - 1] = '=';
      if (mod == 1)
        result[output_length - 2] = '=';
    }

  result[output_length] = '\0';

  return result;
}

char * base64_decode(const char * const src, const size_t input_length)
{
  if (src == NULL)
    return NULL;
  if (input_length % 4 != 0)
    return NULL;

  size_t output_length = (input_length / 4) * 3;
  if (input_length > 0 && src[input_length - 1] == '=')
    output_length--;
  if (input_length > 0 && src[input_length - 2] == '=')
    output_length--;

  char * const result = malloc(output_length + 1);
  if (result == NULL)
    abort();

  size_t i = 0, j = 0;
  for (;i < input_length;)
    {
      const char a = b64_decode_table[src[i++]];
      const char b = b64_decode_table[src[i++]];
      const char c = (src[i] == '=') ? 0 : b64_decode_table[src[i]];
      i++;
      const char d = (src[i] == '=') ? 0 : b64_decode_table[src[i]];
      i++;

      if (a < 0 || b < 0 || c < 0 || d < 0)
        {
          free(result);
          return NULL;
        }

      const int triple = (a << 18) | (b << 12) | (c << 6) | d;

      if (j < output_length)
        result[j++] = (triple >> 16) & 0xFF;
      if (j < output_length)
        result[j++] = (triple >> 8)  & 0xFF;
      if (j < output_length)
        result[j++] = triple         & 0xFF;
    }

  result[output_length] = '\0';

  return result;
}
