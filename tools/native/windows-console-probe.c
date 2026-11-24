/* Test-only console state setter. It is never linked into prismrepl. */
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <windows.h>

static SEXP console_test_set(SEXP settings) {
    if (TYPEOF(settings) != INTSXP || XLENGTH(settings) != 3)
        Rf_error("Expected stdout mode, stderr mode, and output code page.");
    int success = 1;
    if (INTEGER(settings)[0] != NA_INTEGER)
        success &= SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE),
                                  (DWORD) INTEGER(settings)[0]) != 0;
    if (INTEGER(settings)[1] != NA_INTEGER)
        success &= SetConsoleMode(GetStdHandle(STD_ERROR_HANDLE),
                                  (DWORD) INTEGER(settings)[1]) != 0;
    if (INTEGER(settings)[2] != NA_INTEGER)
        success &= SetConsoleOutputCP((UINT) INTEGER(settings)[2]) != 0;
    return Rf_ScalarLogical(success);
}

static SEXP console_test_input_mode(void) {
    DWORD mode = 0;
    if (!GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &mode))
        Rf_error("Cannot inspect console input mode.");
    return Rf_ScalarInteger((int) mode);
}

static SEXP console_test_mouse(SEXP fields) {
    if (TYPEOF(fields) != INTSXP || XLENGTH(fields) != 5)
        Rf_error("Expected mouse x, y, buttons, event flags, and modifiers.");
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (!GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info))
        Rf_error("Cannot inspect the console viewport.");
    INPUT_RECORD record;
    ZeroMemory(&record, sizeof(record));
    record.EventType = MOUSE_EVENT;
    record.Event.MouseEvent.dwMousePosition.X = (SHORT) (info.srWindow.Left + INTEGER(fields)[0] - 1);
    record.Event.MouseEvent.dwMousePosition.Y = (SHORT) (info.srWindow.Top + INTEGER(fields)[1] - 1);
    record.Event.MouseEvent.dwButtonState = (DWORD) INTEGER(fields)[2];
    record.Event.MouseEvent.dwEventFlags = (DWORD) INTEGER(fields)[3];
    record.Event.MouseEvent.dwControlKeyState = (DWORD) INTEGER(fields)[4];
    DWORD written = 0;
    return Rf_ScalarLogical(WriteConsoleInputW(GetStdHandle(STD_INPUT_HANDLE),
        &record, 1, &written) && written == 1);
}

static SEXP console_test_key(SEXP fields) {
    if (TYPEOF(fields) != INTSXP || XLENGTH(fields) != 3)
        Rf_error("Expected virtual key, Unicode character, and modifiers.");
    INPUT_RECORD record;
    ZeroMemory(&record, sizeof(record));
    record.EventType = KEY_EVENT;
    record.Event.KeyEvent.bKeyDown = TRUE;
    record.Event.KeyEvent.wRepeatCount = 1;
    record.Event.KeyEvent.wVirtualKeyCode = (WORD) INTEGER(fields)[0];
    record.Event.KeyEvent.uChar.UnicodeChar = (WCHAR) INTEGER(fields)[1];
    record.Event.KeyEvent.dwControlKeyState = (DWORD) INTEGER(fields)[2];
    DWORD written = 0;
    return Rf_ScalarLogical(WriteConsoleInputW(GetStdHandle(STD_INPUT_HANDLE),
        &record, 1, &written) && written == 1);
}

static const R_CallMethodDef methods[] = {
    {"console_test_set", (DL_FUNC) &console_test_set, 1},
    {"console_test_input_mode", (DL_FUNC) &console_test_input_mode, 0},
    {"console_test_mouse", (DL_FUNC) &console_test_mouse, 1},
    {"console_test_key", (DL_FUNC) &console_test_key, 1},
    {NULL, NULL, 0}
};

void attribute_visible R_init_prismprobe(DllInfo *info) {
    R_registerRoutines(info, NULL, methods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}
