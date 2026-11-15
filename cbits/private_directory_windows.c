#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#include <windows.h>
#include <sddl.h>
#include <stdlib.h>
#include <wchar.h>

/* Query the resolved parent's volume, including mount points and junctions. */
static DWORD check_persistent_acls(LPCWSTR parent)
{
    DWORD flags = 0;
    HANDLE dir = CreateFileW(parent, FILE_READ_ATTRIBUTES,
                             FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                             NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (dir == INVALID_HANDLE_VALUE)
        return GetLastError();

    BOOL ok = GetVolumeInformationByHandleW(dir, NULL, 0, NULL, NULL, &flags, NULL, 0);
    DWORD error = ok ? ERROR_SUCCESS : GetLastError();
    CloseHandle(dir);
    if (error != ERROR_SUCCESS)
        return error;
    return (flags & FILE_PERSISTENT_ACLS) ? ERROR_SUCCESS : ERROR_NOT_SUPPORTED;
}

DWORD hf_create_private_directory(LPCWSTR parent, LPCWSTR path)
{
    DWORD error = check_persistent_acls(parent);
    if (error != ERROR_SUCCESS)
        return error;

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
    error = CreateDirectoryW(path, &attributes) ? ERROR_SUCCESS : GetLastError();

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
