#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <glib.h>
#include "testbus.h"

int main(int argc, char ** argv) {
    TestBusComTestName * proxy;
    GError * error;
    gchar * buf[32];

    error = NULL;
    proxy = test_bus_com_test_name_proxy_new_for_bus_sync(G_BUS_TYPE_SESSION, G_DBUS_PROXY_FLAGS_NONE, "com.test.name", "/com/test/name", NULL, &error);
    
    test_bus_com_test_name_call_hello_world_sync(proxy, "SUPER LIGMA", buf, NULL, &error);
    g_print("Received: %s\n", buf[0]);

    g_object_unref(proxy);
    return(EXIT_SUCCESS);
}
