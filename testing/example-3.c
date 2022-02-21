#include<stdio.h>
#include<gtk/gtk.h>

int ligma = 0;

static void print_hello(GtkWidget *widget, gpointer data) {
    char m[50];
    sprintf(m, "Hello world! %d\n", ligma++);
    g_print(m);
    printf("%d\n", ligma);
}

int main(int argc, char *argv[]) {
    GtkBuilder *builder;
    GObject *window;
    GObject *button;
    GError *error = NULL;
    
    gtk_init(&argc, &argv);
    
    builder = gtk_builder_new();
    if (gtk_builder_add_from_file(builder, "/home/zzapotheosis/Development/GitHub/Personal-Projects/Testing/test.glade", &error) == 0) {
        g_printerr("Error loading file: %s\n", error->message);
        g_clear_error(&error);
        return 1;
    }
    
    window = gtk_builder_get_object(builder, "window1");
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    
    button = gtk_builder_get_object(builder, "button1");
    g_signal_connect(button, "clicked", G_CALLBACK(print_hello), NULL);
    
    gtk_main();
    
    return 0;
}

