#include <CommonCrypto/CommonDigest.h>
#include <errno.h>
#include <unistd.h>

/* Read the already-open input with a bounded buffer. CommonCrypto selects
   the SHA-256 implementation for the running Mac. */
int hf_sha256_fd(int fd, unsigned char digest[CC_SHA256_DIGEST_LENGTH])
{
    CC_SHA256_CTX ctx;
    unsigned char buffer[64 * 1024];
    CC_SHA256_Init(&ctx);
    for (;;) {
        ssize_t count = read(fd, buffer, sizeof(buffer));
        if (count > 0) {
            CC_SHA256_Update(&ctx, buffer, (CC_LONG)count);
        } else if (count == 0) {
            CC_SHA256_Final(digest, &ctx);
            return 0;
        } else if (errno != EINTR) {
            return -1;
        }
    }
}
