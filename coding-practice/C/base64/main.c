#include <stdlib.h>
#include <glib-2.0/glib.h>
#include <glib-2.0/glib/gprintf.h>

/*
 * NOTE: This implementation uses the GLib library from the
 * GNOME Foundation's GNOME Project. Needless to say, this
 * implementation is generally only available on *nix systems.
 *
 * https://docs.gtk.org/glib/index.html
 *
 * NOTE: base64 encoding is NOT the same as encryption! If you
 * need a solid cryptography library, libgcrypt is my personal
 * favorite cryptographic library to use.
 */

int main(int argc, char ** argv) {
    guint8 data[] = "Here is a sample string which we will encode using the "
                    "base64 encoding scheme. If there's anybody even reading "
                    "this, go play NieR:Automata, one of my favorite games.";
    g_printf("Original text:\n%s\n", data);
    guint8 * encoded_data = g_base64_encode(data, sizeof(data));
    g_printf("\nEncoded text:\n%s\n", encoded_data);
    gsize decoded_data_len = 0;
    guint8 * decoded_data = g_base64_decode(encoded_data, &decoded_data_len);
    g_printf("\nDecoded text:\n%s\n", decoded_data);
    g_free(encoded_data);
    g_free(decoded_data);
    return(EXIT_SUCCESS);
}
