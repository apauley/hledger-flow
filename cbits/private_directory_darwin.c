#include <stddef.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

/* Darwin ACL grants can authorize access independently of the Unix mode. */
int hf_verify_private_acl(int fd)
{
    filesec_t security = filesec_init();
    if (security == NULL)
        return -1;
    struct stat status;
    int has_acl = 0;
    int result = fstatx_np(fd, &status, security);
    if (result == 0)
        result = filesec_query_property(security, FILESEC_ACL, &has_acl);
    int error = errno;
    filesec_free(security);
    if (result != 0 || has_acl) {
        errno = result != 0 ? error : EACCES;
        return -1;
    }
    return 0;
}
