#include <stdlib.h>
#include <stdio.h>
#include <gtk/gtk.h>

static void on_activate(GtkApplication * app, gpointer user_data)
{
    GtkBuilder * gtk_builder = gtk_builder_new();
    gtk_builder_add_from_file(gtk_builder, "builder.ui", NULL);

    GObject * main_window = gtk_builder_get_object(gtk_builder, "main_window");
    gtk_window_set_application(GTK_WINDOW(main_window), app);
    
    gtk_window_present(GTK_WINDOW(main_window));
}

int main(int argc, char ** argv)
{
    int exit_code = 0;
    GtkApplication * app = gtk_application_new("lan.home.MyApp", G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app, "activate", G_CALLBACK(on_activate), NULL);
    exit_code = g_application_run(G_APPLICATION(app), argc, argv);
    g_object_unref(app);
    return(exit_code);
}

