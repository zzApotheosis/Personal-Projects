#include <stdlib.h>
#include <stdio.h>
#include <gtk/gtk.h>

static void print_hello(GtkWidget* widget, gpointer data) {
    g_print("Hello World\n");

    if (data != NULL) {
        void (*call_data)(void) = data;
        call_data();
    } else {
        fprintf(stderr, "data is null\n");
    }
}

static void custom_function(void) {
    fprintf(stderr, "Reached custom function!\n");
}

static void activate(GtkApplication* app, gpointer user_data) {
    GtkWidget * window = NULL;
    GtkWidget * button = NULL;
    
    window = gtk_application_window_new(app);
    gtk_window_set_title(GTK_WINDOW(window), "Window");
    gtk_window_set_default_size(GTK_WINDOW(window), 200, 200);
    
    button = gtk_button_new_with_label("Hello World");
    g_signal_connect(button, "clicked", G_CALLBACK(print_hello), custom_function);
    gtk_window_set_child(GTK_WINDOW(window), button);

    gtk_window_present(GTK_WINDOW(window));
}

int main(int argc, char** argv) {
    GtkApplication* app;
    int status;

    app = gtk_application_new("com.ligma", G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
    status = g_application_run(G_APPLICATION(app), argc, argv);

    g_object_unref(app);

    return status;
}

