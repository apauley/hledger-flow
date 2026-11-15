#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#include <windows.h>
#include <sddl.h>
#include <stdlib.h>
#include <wchar.h>

/* Check the volume of the newly created directory before any copies are made. */
static DWORD check_persistent_acls(LPCWSTR path)
{
    DWORD flags = 0;
    HANDLE dir = CreateFileW(path, FILE_READ_ATTRIBUTES,
                             FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                             NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    if (dir == INVALID_HANDLE_VALUE)
        return GetLastError();

    BOOL ok = GetVolumeInformationByHandleW(dir, NULL, 0, NULL, NULL, &flags, NULL, 0);
    DWORD error = ok ? ERROR_SUCCESS : GetLastError();
    CloseHandle(dir);
    if (error != ERROR_SUCCESS)
        return error;
    return (flags & FILE_PERSISTENT_ACLS) ? ERROR_SUCCESS : ERROR_NOT_SUPPORTED;
}

DWORD hf_create_private_directory(LPCWSTR path)
{
    DWORD error;

    HANDLE token = NULL;
    TOKEN_USER *user = NULL;
    LPWSTR sid = NULL;
    LPWSTR sddl = NULL;
    PSECURITY_DESCRIPTOR descriptor = NULL;
    SECURITY_ATTRIBUTES attributes = {sizeof(SECURITY_ATTRIBUTES), NULL, FALSE};
    DWORD size = 0;
    const WCHAR prefix[] = L"D:P(A;OICI;FA;;;";

    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token))
        return GetLastError();

    GetTokenInformation(token, TokenUser, NULL, 0, &size);
    error = GetLastError();
    if (error != ERROR_INSUFFICIENT_BUFFER)
        goto done;
    user = malloc(size);
    if (user == NULL) {
        error = ERROR_OUTOFMEMORY;
        goto done;
    }
    if (!GetTokenInformation(token, TokenUser, user, size, &size) ||
        !ConvertSidToStringSidW(user->User.Sid, &sid)) {
        error = GetLastError();
        goto done;
    }
    sddl = malloc(sizeof(WCHAR) * (wcslen(prefix) + wcslen(sid) + 2));
    if (sddl == NULL) {
        error = ERROR_OUTOFMEMORY;
        goto done;
    }
    wcscpy(sddl, prefix);
    wcscat(sddl, sid);
    wcscat(sddl, L")");
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(
            sddl, SDDL_REVISION_1, &descriptor, NULL)) {
        error = GetLastError();
        goto done;
    }
    attributes.lpSecurityDescriptor = descriptor;
    if (CreateDirectoryW(path, &attributes)) {
        error = check_persistent_acls(path);
        if (error != ERROR_SUCCESS)
            RemoveDirectoryW(path);
    } else {
        error = GetLastError();
    }

done:
    if (descriptor != NULL)
        LocalFree(descriptor);
    free(sddl);
    if (sid != NULL)
        LocalFree(sid);
    free(user);
    CloseHandle(token);
    return error;
}
