#include <gtk/gtk.h>

static const char main_window_description[] =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<interface>\n"
"  <object id=\"window\" class=\"GtkWindow\">\n"
"    <property name=\"title\">Grid</property>\n"
"    <property name=\"default-width\">250</property>\n"
"    <property name=\"default-height\">250</property>\n"
"    <child type=\"titlebar\">\n"
"      <object class=\"GtkHeaderBar\">\n"
"        <child type=\"end\">\n"
"          <object class=\"GtkMenuButton\">\n"
"            <property name=\"direction\">none</property>\n"
"          </object>\n"
"        </child>\n"
"      </object>\n"
"    </child>\n"
"    <child>\n"
"      <object id=\"grid\" class=\"GtkGrid\">\n"
"        <property name=\"row-homogeneous\">TRUE</property>\n"
"        <property name=\"column-homogeneous\">TRUE</property>\n"
"        <child>\n"
"          <object id=\"button1\" class=\"GtkButton\">\n"
"            <property name=\"label\">Button 1</property>\n"
"            <layout>\n"
"              <property name=\"column\">1</property>\n"
"              <property name=\"row\">0</property>\n"
"            </layout>\n"
"          </object>\n"
"        </child>\n"
"        <child>\n"
"          <object id=\"button2\" class=\"GtkButton\">\n"
"            <property name=\"label\">Button 2</property>\n"
"            <layout>\n"
"              <property name=\"column\">1</property>\n"
"              <property name=\"row\">1</property>\n"
"            </layout>\n"
"          </object>\n"
"        </child>\n"
"        <child>\n"
"          <object id=\"quit\" class=\"GtkButton\">\n"
"            <property name=\"label\">Quit</property>\n"
"            <layout>\n"
"              <property name=\"column\">0</property>\n"
"              <property name=\"row\">0</property>\n"
"              <property name=\"row-span\">2</property>\n"
"            </layout>\n"
"          </object>\n"
"        </child>\n"
"      </object>\n"
"    </child>\n"
"  </object>\n"
"</interface>\n"
;

static void
print_hello (GtkWidget * widget, gpointer data) {
    g_print ("Widget received: %s\n", gtk_button_get_label(GTK_BUTTON(widget)));
}

static void
test_label_callback(GSimpleAction * simple, GVariant * parameter, gpointer user_data) {
    g_print("TEST LABEL CLICKED\n");
}

static void
activate (GtkApplication * app, gpointer user_data) {
    GtkBuilder * gtk_builder = gtk_builder_new_from_string(main_window_description, -1);
    
    GtkWidget * main_window = GTK_WIDGET(gtk_builder_get_object(gtk_builder, "window"));
    gtk_window_set_application(GTK_WINDOW(main_window), app);

    GtkWidget * button = GTK_WIDGET(gtk_builder_get_object(gtk_builder, "button1"));
    g_signal_connect(button, "clicked", G_CALLBACK(print_hello), NULL);

    button = GTK_WIDGET(gtk_builder_get_object(gtk_builder, "button2"));
    g_signal_connect(button, "clicked", G_CALLBACK(print_hello), NULL);

    button = GTK_WIDGET(gtk_builder_get_object(gtk_builder, "quit"));
    g_signal_connect_swapped(button, "clicked", G_CALLBACK(gtk_window_close), main_window);

    /* Show main window */
    gtk_widget_show (main_window);
    g_object_unref(gtk_builder);
}

static void activate2(GtkApplication * app, gpointer user_data) {
    GtkWidget * window = gtk_window_new();
    gtk_window_set_application(GTK_WINDOW(window), app);
    gtk_window_set_default_size(GTK_WINDOW(window), 250, 250);
    
    GtkWidget * header_bar = gtk_header_bar_new();
    gtk_window_set_titlebar(GTK_WINDOW(window), header_bar);

    GtkWidget * menu_button = gtk_menu_button_new();
    gtk_header_bar_pack_end(GTK_HEADER_BAR(header_bar), menu_button);
    
    GtkWidget * popover_menu = gtk_popover_menu_new_from_model(NULL);
    gtk_menu_button_set_popover(GTK_MENU_BUTTON(menu_button), popover_menu);
    GMenu * menu = g_menu_new();
    g_menu_append(G_MENU(menu), "Test Label", "app.test");
    gtk_popover_menu_set_menu_model(GTK_POPOVER_MENU(popover_menu), G_MENU_MODEL(menu));

    GMenuItem * menu_item = g_menu_item_new_from_model(G_MENU_MODEL(menu), 0);

    GSimpleAction * test_label_action = g_simple_action_new("test", NULL);
    g_signal_connect(test_label_action, "activated", G_CALLBACK(test_label_callback), GTK_WINDOW(window));
    g_action_map_add_action(G_ACTION_MAP(window), G_ACTION(test_label_action));
    
    g_signal_connect(menu_item, "activate", G_CALLBACK(print_hello), NULL);

    gtk_widget_show(window);
}

int run (int argc, char ** argv) {
    GtkApplication * app;
    int status;

    app = gtk_application_new ("org.gtk.example", G_APPLICATION_FLAGS_NONE);
    g_signal_connect (app, "activate", G_CALLBACK (activate2), NULL);
    status = g_application_run (G_APPLICATION (app), argc, argv);
    g_object_unref (app);

    return status;
}
