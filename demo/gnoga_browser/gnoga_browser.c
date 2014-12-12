#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

static void On_Destroy (GtkWidget* widget, GtkWidget* window);
static gboolean On_Close (WebKitWebView* webView, GtkWidget* window);

int main (int argc, char* argv[])
{
    gtk_init (&argc, &argv);

    GtkWidget *Main_Window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size (GTK_WINDOW (Main_Window), 800, 600);

    WebKitWebView *Web_View = WEBKIT_WEB_VIEW (webkit_web_view_new ());

    gtk_container_add (GTK_CONTAINER (Main_Window), GTK_WIDGET (Web_View));

    g_signal_connect (Main_Window, "destroy", G_CALLBACK (On_Destroy), NULL);
    g_signal_connect (Web_View, "close", G_CALLBACK (On_Close), Main_Window);

    webkit_web_view_load_uri (Web_View, "http://127.0.0.1:8080");
    // Better to use local IP then localhost.

    gtk_widget_grab_focus (GTK_WIDGET (Web_View));

    gtk_widget_show_all (Main_Window);

    gtk_main();

    return 0;
}

static void On_Destroy (GtkWidget* widget, GtkWidget* window)
{
    gtk_main_quit();
}

static gboolean On_Close (WebKitWebView* webView, GtkWidget* window)
{
    gtk_widget_destroy(window);
    return TRUE;
}
