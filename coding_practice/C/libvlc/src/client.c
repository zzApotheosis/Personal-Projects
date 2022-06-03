#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <vlc/vlc.h>

int main(int argc, char ** argv) {
    int status = 0;
    libvlc_instance_t * vlc_instance = libvlc_new(0, NULL);
    libvlc_media_t * vlc_media = libvlc_media_new_location(vlc_instance, "http://127.0.0.1:8080");

    libvlc_media_add_option(vlc_media, ":network-caching=1000");

    libvlc_media_player_t * vlc_media_player = libvlc_media_player_new_from_media(vlc_media);
    
    status = libvlc_media_player_play(vlc_media_player);
    fprintf(stdout, "STATUS = %d\n", status);

    usleep(10000000);

    while (libvlc_media_player_is_playing(vlc_media_player)) {
        usleep(1000000);
    }
    
    libvlc_media_player_stop(vlc_media_player);
    
    libvlc_media_release(vlc_media);
    libvlc_media_player_release(vlc_media_player);
    libvlc_release(vlc_instance);

    return(EXIT_SUCCESS);
}
