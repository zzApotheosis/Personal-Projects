#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <vlc/vlc.h>

int main(int argc, char ** argv) {
    fprintf(stdout, "LIGMA LUL\n");
    libvlc_instance_t * vlc_instance = libvlc_new(0, NULL);
    libvlc_media_t * vlc_media = libvlc_media_new_location(vlc_instance, "screen://");
    
    libvlc_media_add_option(vlc_media, ":screen-fps=15.0");
    libvlc_media_add_option(vlc_media, ":live-caching=300");
    libvlc_media_add_option(vlc_media, ":sout=#transcode{vcodec=h264,scale=Auto,acodec=none,scodec=none}:http{mux=ffmpeg{mux=flv},dst=:8080/}");

    libvlc_media_player_t * vlc_media_player = libvlc_media_player_new_from_media(vlc_media);
    
    libvlc_media_release(vlc_media);

    libvlc_media_player_play(vlc_media_player);

    usleep(60000000); // 60 seconds

    libvlc_media_player_stop(vlc_media_player);
    libvlc_media_player_release(vlc_media_player);
    libvlc_release(vlc_instance);

    return(EXIT_SUCCESS);
}
