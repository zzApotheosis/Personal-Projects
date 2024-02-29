/*
 * This example code is based on the official WeeChat example found here:
 * https://weechat.org/files/doc/weechat/stable/weechat_plugin_api.en.html
 *
 * If the above link doesn't work (it works today, 2024-02-28), then just
 * trust me bro.
 *
 * Build this project with the provided helper script, build.sh, then
 * copy build/libweechat-plugin-example.so to a plugin directory that is
 * recognized by WeeChat. For the standard user, that directory is probably
 * ${HOME}/.local/share/weechat/plugins
 *
 * It is recommended to rename the shared object to an easy name to work with
 * from the WeeChat interface, such as "double.so". From WeeChat's TUI,
 * type "/plugin load double" to load this example plugin. Then run the newly
 * added /double command with arguments to see the magic happen in your current
 * WeeChat buffer.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <weechat/weechat-plugin.h>

#include "common-macros.h"

WEECHAT_PLUGIN_NAME("double")
WEECHAT_PLUGIN_DESCRIPTION("Test plugin for WeeChat")
WEECHAT_PLUGIN_AUTHOR("Steven Jennings <public@zzapotheosis.mozmail.com>")
WEECHAT_PLUGIN_VERSION("0.1")
WEECHAT_PLUGIN_LICENSE("GPL3")

struct t_weechat_plugin * weechat_plugin = NULL;

int command_double_cb (const void * pointer, void * data, struct t_gui_buffer * buffer, int argc, char * argv[], char * argv_eol[]) {
        if (pointer || data || buffer || argv) {} /* Useless statement to satisfy compiler warnings */

        if (argc > 1) {
                weechat_command(NULL, argv_eol[1]);
                weechat_command(NULL, argv_eol[1]);
        }

        return WEECHAT_RC_OK;
}

int weechat_plugin_init(struct t_weechat_plugin * plugin, int argc, char * argv[]) {
        if (plugin == NULL)
                warn(stderr, "Plugin is NULL! Expect a crash soon!");

        if (argc || argv) {} /* Useless statement to satisfy compiler warnings */

        weechat_plugin = plugin;

        weechat_hook_command ("double",
                        "Display two times a message "
                        "or execute two times a command",
                        "message | command",
                        "message: message to display two times\n"
                        "command: command to execute two times",
                        NULL,
                        &command_double_cb, NULL, NULL);
        return WEECHAT_RC_OK;
}

int weechat_plugin_end(struct t_weechat_plugin * plugin) {
        if (plugin) {} /* Useless statement to satisfy compiler warnings */
        return WEECHAT_RC_OK;
}
