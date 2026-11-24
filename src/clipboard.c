#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <limits.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#endif

/* Clipboard access happens only after an explicit selection or paste gesture.
   UTF-16 conversion is independent of the active Windows console code page. */
static SEXP clipboard_result(int ok, const char *text, int length, const char *reason) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, Rf_mkChar("ok"));
    SET_STRING_ELT(names, 1, Rf_mkChar("text"));
    SET_STRING_ELT(names, 2, Rf_mkChar("reason"));
    SET_VECTOR_ELT(result, 0, Rf_ScalarLogical(ok));
    SET_VECTOR_ELT(result, 1, Rf_ScalarString(Rf_mkCharLenCE(text, length, CE_UTF8)));
    SET_VECTOR_ELT(result, 2, Rf_mkString(reason));
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

SEXP C_clipboard_read(SEXP maximum) {
    if (TYPEOF(maximum) != INTSXP || XLENGTH(maximum) != 1 ||
        INTEGER(maximum)[0] < 1 || INTEGER(maximum)[0] > 8388608)
        Rf_error("Clipboard limit must be an integer between 1 and 8388608 bytes.");
#ifdef _WIN32
    int limit = INTEGER(maximum)[0];
    char *buffer = (char *) R_alloc((size_t) limit + 1U, 1);
    if (!IsClipboardFormatAvailable(CF_UNICODETEXT))
        return clipboard_result(0, "", 0, "The clipboard has no Unicode text.");
    if (!OpenClipboard(NULL))
        return clipboard_result(0, "", 0, "The Windows clipboard is busy.");
    HANDLE data = GetClipboardData(CF_UNICODETEXT);
    if (data == NULL) {
        CloseClipboard();
        return clipboard_result(0, "", 0, "Windows could not read the clipboard.");
    }
    SIZE_T available = GlobalSize(data) / sizeof(WCHAR);
    const WCHAR *wide = (const WCHAR *) GlobalLock(data);
    if (wide == NULL) {
        CloseClipboard();
        return clipboard_result(0, "", 0, "Windows could not access clipboard text.");
    }
    SIZE_T count = 0;
    while (count < available && count <= (SIZE_T) limit && wide[count] != L'\0') ++count;
    int bytes = count == 0 ? 0 : (count <= (SIZE_T) limit && count < available ?
        WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, (int) count,
                            NULL, 0, NULL, NULL) : 0);
    int ok = count < available && (count == 0 || (bytes > 0 && bytes <= limit));
    if (ok && bytes > 0)
        ok = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wide, (int) count,
                                 buffer, bytes, NULL, NULL) == bytes;
    GlobalUnlock(data);
    CloseClipboard();
    if (!ok) return clipboard_result(0, "", 0,
        "Clipboard text is too large or contains invalid Unicode; nothing was pasted.");
    buffer[bytes] = '\0';
    return clipboard_result(1, buffer, bytes, "");
#else
    return clipboard_result(0, "", 0, "The native clipboard is available on Windows only.");
#endif
}

SEXP C_clipboard_write(SEXP text) {
    if (TYPEOF(text) != STRSXP || XLENGTH(text) != 1 || STRING_ELT(text, 0) == NA_STRING)
        Rf_error("Clipboard text must be one non-missing string.");
    const char *utf8 = Rf_translateCharUTF8(STRING_ELT(text, 0));
    size_t length = strlen(utf8);
    if (length > 8388608U) Rf_error("Clipboard text must not exceed 8388608 bytes.");
#ifdef _WIN32
    int count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8, -1, NULL, 0);
    if (count <= 0) return clipboard_result(0, "", 0, "Clipboard text is not valid UTF-8.");
    HGLOBAL storage = GlobalAlloc(GMEM_MOVEABLE, (SIZE_T) count * sizeof(WCHAR));
    if (storage == NULL) return clipboard_result(0, "", 0, "Windows could not allocate clipboard text.");
    WCHAR *wide = (WCHAR *) GlobalLock(storage);
    if (wide == NULL || !MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8, -1, wide, count)) {
        if (wide != NULL) GlobalUnlock(storage);
        GlobalFree(storage);
        return clipboard_result(0, "", 0, "Windows could not prepare clipboard text.");
    }
    GlobalUnlock(storage);
    /* EmptyClipboard requires an owner window before SetClipboardData. A tiny
       message-only window avoids depending on a visible console window in ConPTY. */
    HWND owner = CreateWindowExW(0, L"STATIC", L"prismrepl clipboard", 0,
        0, 0, 0, 0, HWND_MESSAGE, NULL, GetModuleHandleW(NULL), NULL);
    if (owner == NULL || !OpenClipboard(owner)) {
        if (owner != NULL) DestroyWindow(owner);
        GlobalFree(storage);
        return clipboard_result(0, "", 0, "The Windows clipboard is busy or unavailable.");
    }
    int ok = EmptyClipboard() && SetClipboardData(CF_UNICODETEXT, storage) != NULL;
    CloseClipboard();
    DestroyWindow(owner);
    if (!ok) GlobalFree(storage); /* The system owns successful transfers. */
    return clipboard_result(ok, "", 0, ok ? "" : "Windows could not copy the selected text.");
#else
    (void) utf8;
    return clipboard_result(0, "", 0, "The native clipboard is available on Windows only.");
#endif
}
