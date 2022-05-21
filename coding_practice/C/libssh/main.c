#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <libssh/libssh.h>

int verify_knownhost(ssh_session session) {
    enum ssh_known_hosts_e state;
    unsigned char *hash = NULL;
    ssh_key srv_pubkey = NULL;
    size_t hlen;
    char buf[10];
    char *hexa;
    char *p;
    int cmp;
    int rc;
 
    rc = ssh_get_server_publickey(session, &srv_pubkey);
    if (rc < 0) {
        return -1;
    }
 
    rc = ssh_get_publickey_hash(srv_pubkey,
                                SSH_PUBLICKEY_HASH_SHA1,
                                &hash,
                                &hlen);
    ssh_key_free(srv_pubkey);
    if (rc < 0) {
        return -1;
    }
 
    state = ssh_session_is_known_server(session);
    switch (state) {
        case SSH_KNOWN_HOSTS_OK:
            /* OK */
 
            break;
        case SSH_KNOWN_HOSTS_CHANGED:
            fprintf(stderr, "Host key for server changed: it is now:\n");
            ssh_print_hash(SSH_PUBLICKEY_HASH_SHA256, hash, hlen);
            fprintf(stderr, "For security reasons, connection will be stopped\n");
            ssh_clean_pubkey_hash(&hash);
 
            return -1;
        case SSH_KNOWN_HOSTS_OTHER:
            fprintf(stderr, "The host key for this server was not found but an other"
                    "type of key exists.\n");
            fprintf(stderr, "An attacker might change the default server key to"
                    "confuse your client into thinking the key does not exist\n");
            ssh_clean_pubkey_hash(&hash);
 
            return -1;
        case SSH_KNOWN_HOSTS_NOT_FOUND:
            fprintf(stderr, "Could not find known host file.\n");
            fprintf(stderr, "If you accept the host key here, the file will be"
                    "automatically created.\n");
 
            /* FALL THROUGH to SSH_SERVER_NOT_KNOWN behavior */
 
        case SSH_KNOWN_HOSTS_UNKNOWN:
            hexa = ssh_get_hexa(hash, hlen);
            fprintf(stderr,"The server is unknown. Do you trust the host key?\n");
            fprintf(stderr, "Public key hash: %s\n", hexa);
            ssh_string_free_char(hexa);
            ssh_clean_pubkey_hash(&hash);
            p = fgets(buf, sizeof(buf), stdin);
            if (p == NULL) {
                return -1;
            }
 
            cmp = strncasecmp(buf, "yes", 3);
            if (cmp != 0) {
                return -1;
            }
 
            rc = ssh_session_update_known_hosts(session);
            if (rc < 0) {
                fprintf(stderr, "Error %s\n", strerror(errno));
                return -1;
            }
 
            break;
        case SSH_KNOWN_HOSTS_ERROR:
            fprintf(stderr, "Error %s", ssh_get_error(session));
            ssh_clean_pubkey_hash(&hash);
            return -1;
    }
 
    ssh_clean_pubkey_hash(&hash);
    return 0;
}

int main(int argc, char ** argv) {
    ssh_session my_ssh_session;
    int rc = 0;
    int port = 22;
    int verbosity = SSH_LOG_PROTOCOL;

    my_ssh_session = ssh_new();
    if (channel == NULL) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        abort();
    }

    ssh_options_set(my_ssh_session, SSH_OPTIONS_USER, "jennings");
    //ssh_options_set(my_ssh_session, SSH_OPTIONS_HOST, "192.168.122.196");
    ssh_options_set(my_ssh_session, SSH_OPTIONS_HOST, "192.168.122.231");
    //ssh_options_set(my_ssh_session, SSH_OPTIONS_KNOWNHOSTS, "/dev/null");
    ssh_options_set(my_ssh_session, SSH_OPTIONS_LOG_VERBOSITY, &verbosity);
    ssh_options_set(my_ssh_session, SSH_OPTIONS_PORT, &port);
    
    rc = ssh_connect(my_ssh_session);
    if (rc != SSH_OK) {
        fprintf(stderr, "Error on ssh_connect(): %s\n", ssh_get_error(my_ssh_session));
        exit(EXIT_FAILURE);
    }
    
    // Verify the server
    if (verify_knownhost(my_ssh_session) < 0) {
        ssh_disconnect(my_ssh_session);
        ssh_free(my_ssh_session);
        exit(EXIT_FAILURE);
    }
    
    // Authenticate this client
    char * password = getpass("Password: ");
    rc = ssh_userauth_password(my_ssh_session, NULL, password);
    if (rc != SSH_AUTH_SUCCESS) {
        fprintf(stderr, "Error authenticating with password\n");
        ssh_disconnect(my_ssh_session);
        ssh_free(my_ssh_session);
        exit(EXIT_FAILURE);
    }

    // DO ACTUAL STUFF
    ssh_channel channel = ssh_channel_new(my_ssh_session);
    if (channel == NULL) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        abort();
    }

    rc = ssh_channel_open_session(channel);
    if (rc != SSH_OK) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        abort();
    }
    
    rc = ssh_channel_request_exec(channel, "cat test.txt");
    if (rc != SSH_OK) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        abort();
    }

    char buffer[256];
    int nbytes;
    nbytes = ssh_channel_read(channel, buffer, sizeof(buffer), 0);
    while (nbytes > 0) {
        if (fwrite(buffer, 1, nbytes, stdout) != nbytes) {
            fprintf(stderr, "Error on line %d\n", __LINE__);
            abort();
        }
        nbytes = ssh_channel_read(channel, buffer, sizeof(buffer), 0);
    }

    if (nbytes < 0) {
        fprintf(stderr, "Error on line %d\n", __LINE__);
        abort();
    }
    ssh_channel_send_eof(channel);
    ssh_channel_close(channel);
    ssh_channel_free(channel);
    
    ssh_disconnect(my_ssh_session);
    ssh_free(my_ssh_session);

    return(EXIT_SUCCESS);
}
