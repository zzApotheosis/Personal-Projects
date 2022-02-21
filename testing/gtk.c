#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <gtk/gtk.h>

static void on_clicked(GtkApplication* app) {
    g_print("Ligma\n");
    return;
}

static void on_activate(GtkApplication* app) {
    GtkWidget* window = gtk_application_window_new(app);
    GtkWidget* button = gtk_button_new_with_label("Hello World!");
    g_signal_connect_swapped(button, "clicked", G_CALLBACK(on_clicked), window);
    gtk_window_set_child(GTK_WINDOW(window), button);
    gtk_window_present(GTK_WINDOW(window));
    return;
}

int main(int argc, char** argv) {
    GtkApplication* app = gtk_application_new("lan.home.GtkApplication", G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app, "activate", G_CALLBACK(on_activate), NULL);
    int status = g_application_run(G_APPLICATION(app), argc, argv);
    g_object_unref(app);
    return status;
}

