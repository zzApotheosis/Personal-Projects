#include <stdlib.h>
#include <stdio.h>
#include <gtk/gtk.h>

static void on_clicked(GtkWidget * widget)
{
    g_print("widget=%d\n", widget);
}

static void on_activate(GtkApplication * app)
{
    g_print("on_activate app = %d\n", app);
    GtkWidget * window = gtk_application_window_new(app);
    GtkWidget * button = gtk_button_new_with_label("Hello world!");
    g_print("window=%d\n", window);
    g_print("button=%d\n", button);
    g_print("on_clicked=%d\n", on_clicked);
    g_signal_connect_swapped(button, "clicked", G_CALLBACK(on_clicked), window);
    gtk_window_set_child(GTK_WINDOW(window), button);
    g_print("first\n");
    gtk_window_present(GTK_WINDOW(window));
    g_print("second\n");
}

int main(int argc, char ** argv)
{
    GtkApplication * app = gtk_application_new("lan.home.MyApp", G_APPLICATION_FLAGS_NONE);
    g_print("before signal connect: %d\n", NULL);
    g_signal_connect(app, "activate", G_CALLBACK(on_activate), NULL);
    return(g_application_run(G_APPLICATION(app), argc, argv));
}

