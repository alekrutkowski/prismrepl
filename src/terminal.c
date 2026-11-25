#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <R_ext/Utils.h>
#include <R_ext/Parse.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include "keyparse.h"

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
#ifndef ENABLE_VIRTUAL_TERMINAL_INPUT
#define ENABLE_VIRTUAL_TERMINAL_INPUT 0x0200
#endif
#else
#include <unistd.h>
#include <termios.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#endif

#define PRISM_PASTE_MAX (1024U * 1024U)
#define PRISM_ESCAPE_WAIT 35
#define TOKEN_TIMEOUT 0
#define TOKEN_BYTE 1
#define TOKEN_KEY 2
#define TOKEN_EOF 3
#define TOKEN_RESIZE 4
#define TOKEN_MOUSE 5

typedef struct {
    int kind;
    unsigned int value;
    prism_key key;
    prism_mouse mouse;
} input_token;

/* Output outlives an individual input line. In particular, evaluation must run
   with VT processing enabled after the line editor restores cooked input. */
typedef struct {
    int active;
    int alternate_screen;
    char cursor_color[8];
    SEXP owner;
#ifdef _WIN32
    HANDLE output, error;
    DWORD output_mode, error_mode;
    int error_is_console;
    UINT output_codepage;
#endif
} output_state;

typedef struct {
    int active;
    int width, height;
    int mouse_enabled;
    SEXP owner;
    input_token pending;
    int has_pending;
    unsigned char utf8[4];
    size_t utf8_length;
    unsigned char *paste;
    size_t paste_length;
    int paste_active, paste_match, paste_truncated;
#ifdef _WIN32
    HANDLE input, output;
    DWORD input_mode;
    DWORD mouse_buttons;
    output_state *owned_output;
    KEY_EVENT_RECORD repeat;
    WORD repeat_left, high_surrogate;
    unsigned int deferred_scalar;
    int has_deferred_scalar;
    unsigned char encoded[4];
    size_t encoded_length, encoded_position;
    INPUT_RECORD records[64];
    DWORD record_length, record_position;
#else
    struct termios saved;
    unsigned char incoming[4096];
    size_t incoming_length, incoming_position;
#endif
} terminal_state;

static terminal_state *active_terminal = NULL;
static output_state *active_output = NULL;

static void close_state(terminal_state *state);

static input_token token(int kind, unsigned int value) {
    input_token result;
    memset(&result, 0, sizeof(result));
    result.kind = kind;
    result.value = value;
    result.key.key = PK_NONE;
    result.key.codepoint = 0;
    result.key.modifiers = 0;
    return result;
}

static void process_events(void) {
#ifdef _WIN32
    R_ProcessEvents();
#endif
    R_CheckUserInterrupt();
}

static void write_control(terminal_state *state, const char *bytes) {
    size_t length = strlen(bytes);
#ifdef _WIN32
    DWORD written;
    if (state->output != INVALID_HANDLE_VALUE)
        (void) WriteFile(state->output, bytes, (DWORD) length, &written, NULL);
#else
    (void) state;
    while (length) {
        ssize_t written = write(STDOUT_FILENO, bytes, length);
        if (written > 0) { bytes += written; length -= (size_t) written; }
        else if (written < 0 && errno == EINTR) continue;
        else break;
    }
#endif
}

static void size_now(int *width, int *height) {
    *width = 80;
    *height = 24;
#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO info;
    HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
    if (GetConsoleScreenBufferInfo(output, &info)) {
        *width = (int) info.srWindow.Right - (int) info.srWindow.Left + 1;
        *height = (int) info.srWindow.Bottom - (int) info.srWindow.Top + 1;
    }
#else
    struct winsize info;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &info) == 0) {
        if (info.ws_col > 0) *width = info.ws_col;
        if (info.ws_row > 0) *height = info.ws_row;
    }
#endif
    if (*width < 1) *width = 80;
    if (*height < 1) *height = 24;
}

static int supported_terminal(void) {
    const char *name = getenv("TERM");
    if (name != NULL && strcmp(name, "dumb") == 0) return 0;
#ifdef _WIN32
    DWORD input_mode, output_mode;
    return GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &input_mode) &&
           GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &output_mode);
#else
    return name != NULL && name[0] != '\0' &&
           isatty(STDIN_FILENO) && isatty(STDOUT_FILENO);
#endif
}

static int supported_output(void) {
    const char *name = getenv("TERM");
    if (name != NULL && strcmp(name, "dumb") == 0) return 0;
#ifdef _WIN32
    DWORD mode;
    return GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &mode);
#else
    return name != NULL && name[0] != '\0' && isatty(STDOUT_FILENO);
#endif
}

/* Cursor tint belongs to the output session, not an individual input line.
   OSC 112 restores the terminal's configured default; it does not query input
   or modify the terminal profile. BEL also works in hosts that do not accept
   ST for this reset. An untouched cursor must not be reset. */
static void reset_cursor_color(output_state *state) {
    if (state == NULL || !state->active || state->cursor_color[0] == '\0') return;
    terminal_state destination;
    memset(&destination, 0, sizeof(destination));
#ifdef _WIN32
    destination.output = state->output;
#endif
    write_control(&destination, "\033]112\007");
    state->cursor_color[0] = '\0';
}

static void close_output_state(output_state *state) {
    if (state == NULL) return;
    if (state->active) {
        reset_cursor_color(state);
        terminal_state destination;
        memset(&destination, 0, sizeof(destination));
#ifdef _WIN32
        destination.output = state->output;
#endif
        write_control(&destination, state->alternate_screen ?
            "\033[?1049l\033[0m\033[?25h" : "\033[0m\033[?25h");
    }
#ifdef _WIN32
    /* Both modes were captured before either was changed. stdout and stderr
       can refer to the same screen buffer through different handles. */
    if (state->error_is_console)
        (void) SetConsoleMode(state->error, state->error_mode);
    (void) SetConsoleMode(state->output, state->output_mode);
    if (state->output_codepage != 0)
        (void) SetConsoleOutputCP(state->output_codepage);
#endif
    if (active_output == state) active_output = NULL;
    if (state->owner != R_NilValue) R_ClearExternalPtr(state->owner);
    free(state);
}

static output_state *open_output_state(SEXP owner) {
    if (!supported_output()) return NULL;
    output_state *state = (output_state *) calloc(1, sizeof(output_state));
    if (state == NULL) Rf_error("Cannot allocate terminal output state.");
    state->owner = owner;
#ifdef _WIN32
    state->output = GetStdHandle(STD_OUTPUT_HANDLE);
    state->error = GetStdHandle(STD_ERROR_HANDLE);
    if (!GetConsoleMode(state->output, &state->output_mode)) {
        free(state);
        return NULL;
    }
    state->error_is_console = GetConsoleMode(state->error, &state->error_mode);
    state->output_codepage = GetConsoleOutputCP();
    DWORD mode = state->output_mode | ENABLE_PROCESSED_OUTPUT |
        ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if (!SetConsoleMode(state->output, mode)) {
        close_output_state(state);
        return NULL;
    }
    state->active = 1;
    if (state->error_is_console &&
        !SetConsoleMode(state->error, state->error_mode | ENABLE_PROCESSED_OUTPUT |
                        ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
        close_output_state(state);
        return NULL;
    }
    /* ReadConsoleInputW does not depend on the input code page. Only output
       needs CP_UTF8 for byte-oriented output written by R and its packages. */
    if (state->output_codepage == 0 || !SetConsoleOutputCP(CP_UTF8)) {
        close_output_state(state);
        return NULL;
    }
#else
    state->active = 1;
#endif
    active_output = state;
    return state;
}

SEXP C_term_output_open(SEXP finalizer) {
    if (!Rf_isFunction(finalizer)) Rf_error("Terminal finalizer must be a function.");
    if (active_output != NULL) Rf_error("A prismrepl output session is already active.");
    if (active_terminal != NULL)
        Rf_error("Open the output session before opening terminal input.");
    if (!supported_output()) return R_NilValue;
    SEXP pointer = PROTECT(R_MakeExternalPtr(NULL,
        Rf_install("prismrepl_output"), R_NilValue));
    R_RegisterFinalizerEx(pointer, finalizer, TRUE);
    output_state *state = open_output_state(pointer);
    if (state != NULL) R_SetExternalPtrAddr(pointer, state);
    UNPROTECT(1);
    return state == NULL ? R_NilValue : pointer;
}

SEXP C_term_output_close(SEXP pointer) {
    if (pointer == R_NilValue) return R_NilValue;
    if (TYPEOF(pointer) != EXTPTRSXP ||
        R_ExternalPtrTag(pointer) != Rf_install("prismrepl_output"))
        Rf_error("Invalid prismrepl output handle.");
    output_state *state = (output_state *) R_ExternalPtrAddr(pointer);
    if (state != NULL && active_output == state) close_state(active_terminal);
    close_output_state(state);
    return R_NilValue;
}

SEXP C_term_output_reset(void) {
    close_state(active_terminal);
    close_output_state(active_output);
    return R_NilValue;
}

SEXP C_term_output_screen(SEXP active) {
    if (TYPEOF(active) != LGLSXP || XLENGTH(active) != 1 || LOGICAL(active)[0] == NA_LOGICAL)
        Rf_error("Alternate-screen ownership must be one non-missing logical.");
    if (active_output == NULL) return Rf_ScalarLogical(FALSE);
    if (!LOGICAL(active)[0]) reset_cursor_color(active_output);
    active_output->alternate_screen = LOGICAL(active)[0];
    return Rf_ScalarLogical(TRUE);
}

/* Render an assembled frame directly, bypassing R's per-write console hook.
   Conversion and allocation finish before synchronized output is activated.
   There is one OS write in the usual case; short writes are completed. */
SEXP C_term_write(SEXP text, SEXP synchronize, SEXP cursor) {
    if (TYPEOF(text) != STRSXP || XLENGTH(text) != 1 || STRING_ELT(text, 0) == NA_STRING)
        Rf_error("Terminal frame must be one non-missing character string.");
    if (TYPEOF(synchronize) != LGLSXP || XLENGTH(synchronize) != 1 ||
        LOGICAL(synchronize)[0] == NA_LOGICAL)
        Rf_error("Synchronized output must be one non-missing logical.");
    char requested_cursor[8] = {0};
    int cursor_change = 0;
    char cursor_control[40] = {0};
    if (cursor != R_NilValue) {
        if (TYPEOF(cursor) != STRSXP || XLENGTH(cursor) != 1 || STRING_ELT(cursor, 0) == NA_STRING)
            Rf_error("Cursor color must be NULL, empty, or a six-digit hex color.");
        const char *color = Rf_translateCharUTF8(STRING_ELT(cursor, 0));
        size_t color_length = strlen(color);
        if (color_length != 0U) {
            if (color_length != 7U || color[0] != '#')
                Rf_error("Cursor color must be NULL, empty, or a six-digit hex color.");
            requested_cursor[0] = '#';
            for (size_t i = 1U; i < 7U; ++i) {
                char digit = color[i];
                if (strchr("0123456789abcdefABCDEF", digit) == NULL)
                    Rf_error("Cursor color must be NULL, empty, or a six-digit hex color.");
                requested_cursor[i] = digit >= 'a' && digit <= 'f' ? (char) (digit - 'a' + 'A') : digit;
            }
            if (active_output == NULL || !active_output->active)
                Rf_error("Cursor color changes require an active terminal output session.");
        }
        cursor_change = active_output != NULL && active_output->active &&
            strcmp(requested_cursor, active_output->cursor_color) != 0;
        if (cursor_change) {
            if (requested_cursor[0] == '\0') memcpy(cursor_control, "\033]112\007", 7U);
            else (void) snprintf(cursor_control, sizeof(cursor_control),
                "\033]12;rgb:%c%c/%c%c/%c%c\007", requested_cursor[1], requested_cursor[2],
                requested_cursor[3], requested_cursor[4], requested_cursor[5], requested_cursor[6]);
        }
    }
    if (!supported_output()) Rf_error("Buffered terminal output requires a real terminal.");
    /* Keep the output owner alive through allocations and UTF-8 conversion. */
    PROTECT(active_output == NULL ? R_NilValue : active_output->owner);
    const char *payload = Rf_translateCharUTF8(STRING_ELT(text, 0));
    size_t payload_length = strlen(payload);
    size_t cursor_length = strlen(cursor_control);
    if (payload_length > (size_t) -1 - cursor_length - 16U)
        Rf_error("Terminal frame is too large.");
    size_t length = payload_length + cursor_length;
    int sync = LOGICAL(synchronize)[0];
    const char *bytes = payload;
    if (cursor_length || (sync && length)) {
        size_t padding = sync ? 8U : 0U;
        char *frame = (char *) R_alloc(length + 2U * padding, 1);
        if (sync) memcpy(frame, "\033[?2026h", padding);
        memcpy(frame + padding, cursor_control, cursor_length);
        memcpy(frame + padding + cursor_length, payload, payload_length);
        if (sync) memcpy(frame + padding + length, "\033[?2026l", padding);
        bytes = frame;
        length += 2U * padding;
    }
    SEXP result = PROTECT(Rf_ScalarReal((double) payload_length));
#ifdef _WIN32
    if (length > INT_MAX) Rf_error("Terminal frame exceeds the Windows console limit.");
    HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
    int count = length ? MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
        bytes, (int) length, NULL, 0) : 0;
    if (length && count <= 0) Rf_error("Terminal frame is not valid UTF-8.");
    WCHAR *wide = (WCHAR *) R_alloc(count ? (size_t) count : 1U, sizeof(WCHAR));
    if (count && MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
        bytes, (int) length, wide, count) != count)
        Rf_error("Cannot convert the terminal frame to Unicode.");
    /* A partial write can already change the cursor. Arm cleanup before I/O;
       clear a reset only after successful completion. */
    if (cursor_change && requested_cursor[0])
        memcpy(active_output->cursor_color, requested_cursor, sizeof(requested_cursor));
    DWORD position = 0;
    while (position < (DWORD) count) {
        DWORD written = 0;
        if (!WriteConsoleW(output, wide + position, (DWORD) count - position,
                           &written, NULL) || written == 0) {
            DWORD error = GetLastError();
            if (sync) {
                DWORD ignored;
                (void) WriteConsoleW(output, L"\033[?2026l", 8, &ignored, NULL);
            }
            Rf_error("Cannot write the terminal frame (Windows error %lu).", (unsigned long) error);
        }
        position += written;
    }
#else
    if (cursor_change && requested_cursor[0])
        memcpy(active_output->cursor_color, requested_cursor, sizeof(requested_cursor));
    size_t position = 0;
    while (position < length) {
        ssize_t written = write(STDOUT_FILENO, bytes + position, length - position);
        if (written > 0) position += (size_t) written;
        else if (written < 0 && errno == EINTR) continue;
        else {
            int error = errno;
            if (sync) write_control(NULL, "\033[?2026l");
            Rf_error("Cannot write the terminal frame: %s.", strerror(error));
        }
    }
#endif
    if (cursor_change && requested_cursor[0] == '\0') active_output->cursor_color[0] = '\0';
    UNPROTECT(2);
    return result;
}

SEXP C_term_output_state(void) {
    const char *labels[] = {"supported", "active", "vt_stdout", "vt_stderr",
        "codepage", "stdout_mode", "stderr_mode"};
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 7));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 7));
    for (int i = 0; i < 7; ++i) SET_STRING_ELT(names, i, Rf_mkChar(labels[i]));
    SET_VECTOR_ELT(result, 0, Rf_ScalarLogical(supported_output()));
    SET_VECTOR_ELT(result, 1, Rf_ScalarLogical(active_output != NULL));
#ifdef _WIN32
    DWORD output_mode = 0, error_mode = 0;
    int output_ok = GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &output_mode);
    int error_ok = GetConsoleMode(GetStdHandle(STD_ERROR_HANDLE), &error_mode);
    UINT codepage = GetConsoleOutputCP();
    SET_VECTOR_ELT(result, 2, Rf_ScalarLogical(output_ok ?
        (output_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0 : NA_LOGICAL));
    SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(error_ok ?
        (error_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0 : NA_LOGICAL));
    SET_VECTOR_ELT(result, 4, Rf_ScalarInteger(codepage ? (int) codepage : NA_INTEGER));
    SET_VECTOR_ELT(result, 5, Rf_ScalarInteger(output_ok ? (int) output_mode : NA_INTEGER));
    SET_VECTOR_ELT(result, 6, Rf_ScalarInteger(error_ok ? (int) error_mode : NA_INTEGER));
#else
    SET_VECTOR_ELT(result, 2, Rf_ScalarLogical(NA_LOGICAL));
    SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(NA_LOGICAL));
    SET_VECTOR_ELT(result, 4, Rf_ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(result, 5, Rf_ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(result, 6, Rf_ScalarInteger(NA_INTEGER));
#endif
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

static void close_state(terminal_state *state) {
    if (state == NULL) return;
    if (state->active) {
        write_control(state, state->mouse_enabled ?
            "\033[?1002l\033[?1000l\033[?1006l\033[?2004l\033[0m\033[?25h" :
            "\033[?2004l\033[0m\033[?25h");
#ifdef _WIN32
        (void) SetConsoleMode(state->input, state->input_mode);
#else
        (void) tcsetattr(STDIN_FILENO, TCSANOW, &state->saved);
#endif
        state->active = 0;
    }
#ifdef _WIN32
    /* Standalone editor use still restores its own temporary output guard. */
    if (state->owned_output != NULL) close_output_state(state->owned_output);
#endif
    if (active_terminal == state) active_terminal = NULL;
    if (state->owner != R_NilValue) {
        R_SetExternalPtrProtected(state->owner, R_NilValue);
        R_ClearExternalPtr(state->owner);
    }
    free(state->paste);
    state->paste = NULL;
    free(state);
}

SEXP C_term_open(SEXP finalizer, SEXP mouse) {
    SEXP pointer;
    terminal_state *state;
    if (!Rf_isFunction(finalizer)) Rf_error("Terminal finalizer must be a function.");
    if (TYPEOF(mouse) != LGLSXP || XLENGTH(mouse) != 1 || LOGICAL(mouse)[0] == NA_LOGICAL)
        Rf_error("Mouse input must be one non-missing logical.");
    if (active_terminal != NULL) Rf_error("A prismrepl terminal is already active.");
    if (!supported_terminal()) return R_NilValue;
    pointer = PROTECT(R_MakeExternalPtr(NULL, Rf_install("prismrepl_terminal"), R_NilValue));
    /* An R finalizer stays safe if a user explicitly unloads this DLL. */
    R_RegisterFinalizerEx(pointer, finalizer, TRUE);
    state = (terminal_state *) calloc(1, sizeof(terminal_state));
    if (state == NULL) Rf_error("Cannot allocate terminal state.");
    state->owner = pointer;
    state->mouse_enabled = LOGICAL(mouse)[0];
    R_SetExternalPtrAddr(pointer, state);
    size_now(&state->width, &state->height);
#ifdef _WIN32
    state->input = GetStdHandle(STD_INPUT_HANDLE);
    state->output = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!GetConsoleMode(state->input, &state->input_mode)) {
        close_state(state); UNPROTECT(1); return R_NilValue;
    }
    DWORD input_mode = (state->input_mode | ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS) &
        ~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT |
          ENABLE_QUICK_EDIT_MODE | ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_MOUSE_INPUT);
    if (state->mouse_enabled) input_mode |= ENABLE_MOUSE_INPUT;
    if (active_output == NULL) state->owned_output = open_output_state(R_NilValue);
    if (active_output == NULL) { close_state(state); UNPROTECT(1); return R_NilValue; }
    if (!SetConsoleMode(state->input, input_mode)) {
        close_state(state); UNPROTECT(1); return R_NilValue;
    }
#else
    struct termios raw;
    if (tcgetattr(STDIN_FILENO, &state->saved) != 0) {
        close_state(state); UNPROTECT(1); return R_NilValue;
    }
    raw = state->saved;
    raw.c_iflag &= (tcflag_t) ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_cflag |= CS8;
    raw.c_lflag &= (tcflag_t) ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0;
    /* Keep output postprocessing: R output still uses ordinary newlines. */
    if (tcsetattr(STDIN_FILENO, TCSANOW, &raw) != 0) {
        close_state(state); UNPROTECT(1); return R_NilValue;
    }
#endif
    if (active_output != NULL && active_output->owner != R_NilValue)
        R_SetExternalPtrProtected(pointer, active_output->owner);
    state->active = 1;
    active_terminal = state;
    write_control(state, state->mouse_enabled ?
        "\033[?2004h\033[?1000h\033[?1002h\033[?1006h" : "\033[?2004h");
    UNPROTECT(1);
    return pointer;
}

static terminal_state *checked_state(SEXP pointer) {
    if (TYPEOF(pointer) != EXTPTRSXP ||
        R_ExternalPtrTag(pointer) != Rf_install("prismrepl_terminal"))
        Rf_error("Invalid prismrepl terminal handle.");
    return (terminal_state *) R_ExternalPtrAddr(pointer);
}

SEXP C_term_close(SEXP pointer) {
    if (pointer != R_NilValue) close_state(checked_state(pointer));
    return R_NilValue;
}

SEXP C_term_reset(void) {
    close_state(active_terminal);
    return R_NilValue;
}

SEXP C_term_size(void) {
    int width, height;
    SEXP result = PROTECT(Rf_allocVector(INTSXP, 2));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
    size_now(&width, &height);
    INTEGER(result)[0] = width;
    INTEGER(result)[1] = height;
    SET_STRING_ELT(names, 0, Rf_mkChar("width"));
    SET_STRING_ELT(names, 1, Rf_mkChar("height"));
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

SEXP C_term_capabilities(void) {
    const char *term = getenv("TERM");
    const char *color = getenv("COLORTERM");
    const char *wt = getenv("WT_SESSION");
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 4));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
    const char *labels[] = {"supported", "platform", "terminal", "truecolor"};
    for (int i = 0; i < 4; ++i) SET_STRING_ELT(names, i, Rf_mkChar(labels[i]));
    SET_VECTOR_ELT(result, 0, Rf_ScalarLogical(supported_terminal()));
#ifdef _WIN32
    SET_VECTOR_ELT(result, 1, Rf_mkString("windows"));
#elif defined(__APPLE__)
    SET_VECTOR_ELT(result, 1, Rf_mkString("macos"));
#else
    SET_VECTOR_ELT(result, 1, Rf_mkString("unix"));
#endif
    SET_VECTOR_ELT(result, 2, Rf_mkString(term == NULL ? "" : term));
    SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(
        (color != NULL && (strstr(color, "truecolor") != NULL || strstr(color, "24bit") != NULL)) ||
        (wt != NULL && wt[0] != '\0')));
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

#ifdef _WIN32
static int windows_modifiers(DWORD flags) {
    return ((flags & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) ? PM_CTRL : 0) |
        ((flags & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)) ? PM_ALT : 0) |
        ((flags & SHIFT_PRESSED) ? PM_SHIFT : 0);
}

static enum prism_mouse_button windows_button(DWORD buttons) {
    if (buttons & FROM_LEFT_1ST_BUTTON_PRESSED) return PB_LEFT;
    if (buttons & FROM_LEFT_2ND_BUTTON_PRESSED) return PB_MIDDLE;
    if (buttons & RIGHTMOST_BUTTON_PRESSED) return PB_RIGHT;
    return PB_NONE;
}

static input_token windows_mouse(terminal_state *state, MOUSE_EVENT_RECORD record) {
    input_token result = token(TOKEN_TIMEOUT, 0);
    CONSOLE_SCREEN_BUFFER_INFO info;
    DWORD buttons = record.dwButtonState &
        (FROM_LEFT_1ST_BUTTON_PRESSED | FROM_LEFT_2ND_BUTTON_PRESSED | RIGHTMOST_BUTTON_PRESSED);
    DWORD previous = state->mouse_buttons;
    state->mouse_buttons = buttons;
    if (!state->mouse_enabled || !GetConsoleScreenBufferInfo(state->output, &info)) return result;
    int x = (int) record.dwMousePosition.X - (int) info.srWindow.Left + 1;
    int y = (int) record.dwMousePosition.Y - (int) info.srWindow.Top + 1;
    int width = (int) info.srWindow.Right - (int) info.srWindow.Left + 1;
    int height = (int) info.srWindow.Bottom - (int) info.srWindow.Top + 1;
    if (x < 1 || y < 1 || x > width || y > height) return result;
    prism_mouse mouse = {1, x, y, PB_NONE, PA_NONE, windows_modifiers(record.dwControlKeyState)};
    if (record.dwEventFlags == MOUSE_WHEELED) {
        SHORT delta = (SHORT) HIWORD(record.dwButtonState);
        if (!delta) return result;
        mouse.button = delta > 0 ? PB_UP : PB_DOWN;
        mouse.action = PA_WHEEL;
    } else if (record.dwEventFlags == MOUSE_MOVED) {
        /* Idle pointer motion does not invalidate a frame. */
        if (!buttons) return result;
        mouse.button = windows_button(buttons);
        mouse.action = PA_MOVE;
    } else if (record.dwEventFlags == 0 || record.dwEventFlags == DOUBLE_CLICK) {
        DWORD pressed = buttons & ~previous;
        DWORD released = previous & ~buttons;
        if (pressed || (record.dwEventFlags == DOUBLE_CLICK && buttons)) {
            mouse.button = windows_button(pressed ? pressed : buttons);
            mouse.action = PA_PRESS;
        } else if (released) {
            mouse.button = windows_button(released);
            mouse.action = PA_RELEASE;
        } else return result;
    } else return result;
    result.kind = TOKEN_MOUSE;
    result.mouse = mouse;
    return result;
}

static enum prism_key windows_key(WORD key) {
    if (key >= VK_F1 && key <= VK_F24)
        return (enum prism_key) (PK_F1 + (key - VK_F1));
    switch (key) {
    case VK_LEFT: return PK_LEFT;
    case VK_RIGHT: return PK_RIGHT;
    case VK_UP: return PK_UP;
    case VK_DOWN: return PK_DOWN;
    case VK_HOME: return PK_HOME;
    case VK_END: return PK_END;
    case VK_PRIOR: return PK_PAGEUP;
    case VK_NEXT: return PK_PAGEDOWN;
    case VK_DELETE: return PK_DELETE;
    case VK_INSERT: return PK_INSERT;
    default: return PK_NONE;
    }
}
#endif

static input_token os_next(terminal_state *state, int timeout) {
    if (state->has_pending) {
        state->has_pending = 0;
        return state->pending;
    }
#ifdef _WIN32
    if (state->encoded_position < state->encoded_length)
        return token(TOKEN_BYTE, state->encoded[state->encoded_position++]);
    if (state->has_deferred_scalar) {
        state->has_deferred_scalar = 0;
        state->encoded_length = prism_utf8_encode(state->deferred_scalar, state->encoded);
        state->encoded_position = 1;
        return token(TOKEN_BYTE, state->encoded[0]);
    }
    for (int attempts = 0; attempts < 128; ++attempts) {
        KEY_EVENT_RECORD record;
        if (state->repeat_left) {
            record = state->repeat;
            --state->repeat_left;
        } else {
            INPUT_RECORD input;
            if (state->record_position == state->record_length) {
                DWORD waited = WaitForSingleObject(state->input, (DWORD) timeout);
                if (waited == WAIT_TIMEOUT) return token(TOKEN_TIMEOUT, 0);
                if (waited != WAIT_OBJECT_0) return token(TOKEN_EOF, 0);
                if (!ReadConsoleInputW(state->input, state->records, 64, &state->record_length))
                    return token(TOKEN_EOF, 0);
                state->record_position = 0;
                if (!state->record_length) return token(TOKEN_TIMEOUT, 0);
            }
            input = state->records[state->record_position++];
            if (input.EventType == WINDOW_BUFFER_SIZE_EVENT) {
                int width, height;
                size_now(&width, &height);
                if (width == state->width && height == state->height) continue;
                state->width = width;
                state->height = height;
                return token(TOKEN_RESIZE, 0);
            }
            if (input.EventType == MOUSE_EVENT) {
                input_token mouse = windows_mouse(state, input.Event.MouseEvent);
                if (mouse.kind == TOKEN_MOUSE) return mouse;
                continue;
            }
            if (input.EventType != KEY_EVENT || !input.Event.KeyEvent.bKeyDown) continue;
            record = input.Event.KeyEvent;
            state->repeat = record;
            state->repeat_left = record.wRepeatCount > 0 ? record.wRepeatCount - 1 : 0;
        }
        unsigned int cp = record.uChar.UnicodeChar;
        DWORD flags = record.dwControlKeyState;
        int mods = windows_modifiers(flags);
        enum prism_key special = windows_key(record.wVirtualKeyCode);
        input_token result = token(TOKEN_KEY, 0);
        result.key.modifiers = mods;
        if (special != PK_NONE) { result.key.key = special; return result; }
        if (record.wVirtualKeyCode == VK_RETURN && !state->paste_active) {
            result.key.key = PK_ENTER;
            return result;
        }
        /* AltGr produces printable text on many layouts. */
        if ((flags & LEFT_CTRL_PRESSED) && (flags & RIGHT_ALT_PRESSED) && cp >= 32U)
            mods &= ~(PM_CTRL | PM_ALT);
        if ((mods & PM_CTRL) && record.wVirtualKeyCode >= 'A' && record.wVirtualKeyCode <= 'Z') {
            result.key.key = PK_CHARACTER;
            result.key.codepoint = record.wVirtualKeyCode + 32U;
            result.key.modifiers = mods;
            return result;
        }
        if (cp == 0 && (mods & (PM_CTRL | PM_ALT))) {
            /* A console record can carry a real shortcut without a character.
               Use the active keyboard layout without changing dead-key state. */
            UINT mapped = MapVirtualKeyExW(record.wVirtualKeyCode, MAPVK_VK_TO_CHAR,
                                          GetKeyboardLayout(0));
            if (!(mapped & 0x80000000U) && mapped >= 32U && mapped < 127U) cp = mapped;
        }
        if ((mods & (PM_CTRL | PM_ALT)) && cp > 0 && cp < 128U) {
            result.key = prism_parse_control(cp);
            result.key.modifiers |= mods;
            if (cp >= 'A' && cp <= 'Z') result.key.codepoint = cp + 32U;
            return result;
        }
        if (record.wVirtualKeyCode == VK_RETURN && mods) {
            result.key.key = PK_ENTER; result.key.modifiers = mods; return result;
        }
        if (record.wVirtualKeyCode == VK_TAB && (mods & PM_SHIFT)) {
            result.key.key = PK_TAB; return result;
        }
        if (cp == 0U) continue;
        if (cp >= 0xd800U && cp <= 0xdbffU) {
            state->high_surrogate = (WORD) cp;
            continue;
        }
        if (cp >= 0xdc00U && cp <= 0xdfffU) {
            cp = state->high_surrogate ?
                0x10000U + (((unsigned int) state->high_surrogate - 0xd800U) << 10) + (cp - 0xdc00U) :
                0xfffdU;
            if (!state->repeat_left) state->high_surrogate = 0;
        } else if (state->high_surrogate) {
            /* Preserve the next scalar when a malformed UTF-16 pair arrives. */
            state->deferred_scalar = cp;
            state->has_deferred_scalar = 1;
            state->high_surrogate = 0;
            cp = 0xfffdU;
        }
        state->encoded_length = prism_utf8_encode(cp, state->encoded);
        state->encoded_position = 1;
        return token(TOKEN_BYTE, state->encoded[0]);
    }
    return token(TOKEN_TIMEOUT, 0);
#else
    if (state->incoming_position < state->incoming_length)
        return token(TOKEN_BYTE, state->incoming[state->incoming_position++]);
    fd_set readset;
    struct timeval delay;
    FD_ZERO(&readset);
    FD_SET(STDIN_FILENO, &readset);
    delay.tv_sec = timeout / 1000;
    delay.tv_usec = (timeout % 1000) * 1000;
    int ready = select(STDIN_FILENO + 1, &readset, NULL, NULL, &delay);
    if (ready < 0) {
        if (errno == EINTR) { process_events(); return token(TOKEN_TIMEOUT, 0); }
        return token(TOKEN_EOF, 0);
    }
    if (ready == 0) return token(TOKEN_TIMEOUT, 0);
    ssize_t received = read(STDIN_FILENO, state->incoming, sizeof(state->incoming));
    if (received < 0 && (errno == EINTR || errno == EAGAIN)) return token(TOKEN_TIMEOUT, 0);
    if (received <= 0) return token(TOKEN_EOF, 0);
    state->incoming_length = (size_t) received;
    state->incoming_position = 1;
    return token(TOKEN_BYTE, state->incoming[0]);
#endif
}

static SEXP event_result(const char *type, prism_key key, const unsigned char *text,
                         size_t length, int truncated) {
    const char *labels[] = {"type", "key", "text", "modifiers", "truncated",
        "x", "y", "button", "action"};
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 9));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 9));
    SEXP modifiers = PROTECT(Rf_allocVector(STRSXP,
        ((key.modifiers & PM_CTRL) != 0) + ((key.modifiers & PM_ALT) != 0) +
        ((key.modifiers & PM_SHIFT) != 0)));
    int i = 0;
    if (key.modifiers & PM_CTRL) SET_STRING_ELT(modifiers, i++, Rf_mkChar("ctrl"));
    if (key.modifiers & PM_ALT) SET_STRING_ELT(modifiers, i++, Rf_mkChar("alt"));
    if (key.modifiers & PM_SHIFT) SET_STRING_ELT(modifiers, i++, Rf_mkChar("shift"));
    for (i = 0; i < 9; ++i) SET_STRING_ELT(names, i, Rf_mkChar(labels[i]));
    SET_VECTOR_ELT(result, 0, Rf_mkString(type));
    if (key.key == PK_CHARACTER) {
        unsigned char encoded[4];
        size_t count = prism_utf8_encode(key.codepoint, encoded);
        SET_VECTOR_ELT(result, 1, Rf_ScalarString(Rf_mkCharLenCE((const char *) encoded, (int) count, CE_UTF8)));
    } else SET_VECTOR_ELT(result, 1, Rf_mkString(prism_key_name(key.key)));
    SET_VECTOR_ELT(result, 2, Rf_ScalarString(Rf_mkCharLenCE(
        text == NULL ? "" : (const char *) text, (int) length, CE_UTF8)));
    SET_VECTOR_ELT(result, 3, modifiers);
    SET_VECTOR_ELT(result, 4, Rf_ScalarLogical(truncated));
    SET_VECTOR_ELT(result, 5, Rf_ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(result, 6, Rf_ScalarInteger(NA_INTEGER));
    SET_VECTOR_ELT(result, 7, Rf_mkString(""));
    SET_VECTOR_ELT(result, 8, Rf_mkString(""));
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(3);
    return result;
}

static SEXP mouse_result(prism_mouse mouse) {
    const char *buttons[] = {"none", "left", "middle", "right", "up", "down"};
    const char *keys[] = {"", "left", "middle", "right", "wheelup", "wheeldown"};
    const char *actions[] = {"", "press", "release", "move", "wheel"};
    prism_key key = {PK_NONE, 0, mouse.modifiers};
    SEXP result = PROTECT(event_result("mouse", key, NULL, 0, 0));
    SET_VECTOR_ELT(result, 1, Rf_mkString(keys[mouse.button]));
    SET_VECTOR_ELT(result, 5, Rf_ScalarInteger(mouse.x));
    SET_VECTOR_ELT(result, 6, Rf_ScalarInteger(mouse.y));
    SET_VECTOR_ELT(result, 7, Rf_mkString(buttons[mouse.button]));
    SET_VECTOR_ELT(result, 8, Rf_mkString(actions[mouse.action]));
    UNPROTECT(1);
    return result;
}

static SEXP empty_event(const char *type) {
    prism_key key = {PK_NONE, 0, 0};
    return event_result(type, key, NULL, 0, 0);
}

static SEXP key_result(prism_key key) {
    if (key.key == PK_NONE) return empty_event("timeout");
    if (key.key == PK_CHARACTER && (key.modifiers & (PM_CTRL | PM_ALT)) == 0) {
        unsigned char encoded[4];
        size_t count = prism_utf8_encode(key.codepoint, encoded);
        return event_result("text", key, encoded, count, 0);
    }
    return event_result("key", key, NULL, 0, 0);
}

static void append_paste(terminal_state *state, unsigned char byte) {
    if (state->paste_length < PRISM_PASTE_MAX)
        state->paste[state->paste_length++] = byte;
    else state->paste_truncated = 1;
}

static SEXP read_paste(terminal_state *state, int timeout) {
    const unsigned char end[] = {27, '[', '2', '0', '1', '~'};
    for (int ticks = 0; ticks < 16384; ++ticks) {
        input_token input = os_next(state, ticks ? 0 : timeout);
        if (input.kind == TOKEN_TIMEOUT) return empty_event("timeout");
        if (input.kind == TOKEN_EOF) return empty_event("eof");
        if (input.kind == TOKEN_RESIZE || input.kind == TOKEN_MOUSE) continue;
        if (input.kind == TOKEN_KEY) {
            if (input.key.key == PK_CHARACTER && input.key.codepoint == 'c' &&
                (input.key.modifiers & PM_CTRL)) {
                state->paste_active = 0; state->paste_length = 0; state->paste_match = 0;
                return key_result(input.key);
            }
            if (input.key.key == PK_ENTER) append_paste(state, '\r');
            else if (input.key.key == PK_CHARACTER && input.key.codepoint == 'j' &&
                     (input.key.modifiers & PM_CTRL)) append_paste(state, '\n');
            continue;
        }
        unsigned char byte = (unsigned char) input.value;
        if (byte == 3) {
            state->paste_active = 0; state->paste_length = 0; state->paste_match = 0;
            return key_result(prism_parse_control(3));
        }
        if (byte == end[state->paste_match]) {
            ++state->paste_match;
            if (state->paste_match == 6) {
                prism_key key = {PK_NONE, 0, 0};
                size_t capacity = state->paste_length * 3U + 1U;
                unsigned char *clean = (unsigned char *) R_alloc(capacity, 1);
                size_t length = prism_utf8_sanitize(state->paste, state->paste_length, clean, capacity);
                state->paste_active = 0;
                state->paste_match = 0;
                state->paste_length = 0;
                return event_result("paste", key, clean, length, state->paste_truncated);
            }
        } else {
            for (int i = 0; i < state->paste_match; ++i) append_paste(state, end[i]);
            state->paste_match = 0;
            if (byte == 27) state->paste_match = 1;
            else append_paste(state, byte);
        }
        if ((ticks & 4095) == 4095) process_events();
    }
    return empty_event("timeout");
}

static prism_key read_escape(terminal_state *state, prism_mouse *mouse) {
    unsigned char sequence[96] = {27};
    size_t length = 1;
    input_token input = os_next(state, PRISM_ESCAPE_WAIT);
    if (input.kind != TOKEN_BYTE) {
        if (input.kind != TOKEN_TIMEOUT) { state->pending = input; state->has_pending = 1; }
        return prism_parse_control(27);
    }
    sequence[length++] = (unsigned char) input.value;
    if (input.value == '[' || input.value == 'O') {
        while (length < sizeof(sequence)) {
            input = os_next(state, PRISM_ESCAPE_WAIT);
            if (input.kind != TOKEN_BYTE) {
                if (input.kind != TOKEN_TIMEOUT) { state->pending = input; state->has_pending = 1; }
                break;
            }
            sequence[length++] = (unsigned char) input.value;
            if (input.value >= 0x40U && input.value <= 0x7eU) break;
        }
    } else if (input.value >= 0x80U) {
        unsigned int scalar;
        size_t consumed;
        while (length < 5 && prism_utf8_decode(sequence + 1, length - 1, &scalar, &consumed) == 0) {
            input = os_next(state, PRISM_ESCAPE_WAIT);
            if (input.kind != TOKEN_BYTE) {
                if (input.kind != TOKEN_TIMEOUT) { state->pending = input; state->has_pending = 1; }
                break;
            }
            if ((input.value & 0xc0U) != 0x80U) {
                state->pending = input; state->has_pending = 1; break;
            }
            sequence[length++] = (unsigned char) input.value;
        }
    }
    *mouse = prism_parse_mouse(sequence, length);
    return prism_parse_escape(sequence, length);
}

typedef struct { terminal_state *state; int timeout; } read_data;

static SEXP read_unicode(terminal_state *state, int timeout) {
    unsigned int scalar;
    size_t consumed;
    int valid;
    while ((valid = prism_utf8_decode(state->utf8, state->utf8_length,
                                      &scalar, &consumed)) == 0 &&
           state->utf8_length < sizeof(state->utf8)) {
        input_token input = os_next(state, timeout);
        if (input.kind == TOKEN_TIMEOUT) return empty_event("timeout");
        if (input.kind == TOKEN_EOF) return empty_event("eof");
        if (input.kind == TOKEN_RESIZE) return empty_event("resize");
        if (input.kind != TOKEN_BYTE || (input.value & 0xc0U) != 0x80U) {
            state->pending = input;
            state->has_pending = 1;
            break;
        }
        state->utf8[state->utf8_length++] = (unsigned char) input.value;
    }
    if (valid != 1) scalar = 0xfffdU;
    state->utf8_length = 0;
    prism_key key = {PK_CHARACTER, scalar, 0};
    return key_result(key);
}

static SEXP read_body(void *data) {
    read_data *call = (read_data *) data;
    terminal_state *state = call->state;
    int width, height;
    process_events();
    size_now(&width, &height);
    if (width != state->width || height != state->height) {
        state->width = width; state->height = height;
        return empty_event("resize");
    }
    if (state->paste_active) return read_paste(state, call->timeout);
    if (state->utf8_length) return read_unicode(state, call->timeout);
    input_token input = os_next(state, call->timeout);
    if (input.kind == TOKEN_TIMEOUT) { process_events(); return empty_event("timeout"); }
    if (input.kind == TOKEN_EOF) return empty_event("eof");
    if (input.kind == TOKEN_RESIZE) return empty_event("resize");
    if (input.kind == TOKEN_KEY) return key_result(input.key);
    if (input.kind == TOKEN_MOUSE) return mouse_result(input.mouse);
    if (input.value == 27) {
        prism_mouse mouse = {0, 0, 0, PB_NONE, PA_NONE, 0};
        prism_key key = read_escape(state, &mouse);
        if (mouse.valid) return state->mouse_enabled ? mouse_result(mouse) : empty_event("timeout");
        if (key.key == PK_PASTE_START) {
            if (state->paste == NULL) state->paste = (unsigned char *) malloc(PRISM_PASTE_MAX);
            if (state->paste == NULL) Rf_error("Cannot allocate paste buffer.");
            state->paste_active = 1;
            state->paste_length = 0;
            state->paste_match = 0;
            state->paste_truncated = 0;
            return read_paste(state, call->timeout);
        }
        return key_result(key);
    }
    if (input.value < 128U) return key_result(prism_parse_control(input.value));
    state->utf8[0] = (unsigned char) input.value;
    state->utf8_length = 1;
    return read_unicode(state, call->timeout);
}

static void read_cleanup(void *data, Rboolean jump) {
    if (jump) close_state(((read_data *) data)->state);
}

SEXP C_term_read(SEXP pointer, SEXP timeout) {
    terminal_state *state = checked_state(pointer);
    int milliseconds = Rf_asInteger(timeout);
    if (milliseconds == NA_INTEGER || milliseconds < 0 || milliseconds > 1000)
        Rf_error("Terminal timeout must be between 0 and 1000 milliseconds.");
    if (state == NULL || !state->active) Rf_error("The terminal handle is closed.");
    read_data data = {state, milliseconds};
    return R_UnwindProtect(read_body, &data, read_cleanup, &data, NULL);
}

SEXP C_parse_status(SEXP text) {
    ParseStatus status;
    if (TYPEOF(text) != STRSXP || XLENGTH(text) != 1 || STRING_ELT(text, 0) == NA_STRING)
        Rf_error("Code must be one non-missing character string.");
    SEXP expression = PROTECT(R_ParseVector(text, -1, &status, R_NilValue));
    const char *label;
    switch (status) {
    case PARSE_OK: label = XLENGTH(expression) ? "complete" : "null"; break;
    case PARSE_NULL: case PARSE_EOF: label = "null"; break;
    case PARSE_INCOMPLETE: label = "incomplete"; break;
    default: label = "error"; break;
    }
    (void) expression;
    SEXP result = PROTECT(Rf_mkString(label));
    UNPROTECT(2);
    return result;
}

#if R_VERSION < R_Version(4, 6, 0)
typedef struct {
    SEXP environment;
    SEXP symbol;
    int missing;
} binding_value_query;

static SEXP binding_value_read(void *data) {
    binding_value_query *query = (binding_value_query *) data;
    return R_getVar(query->symbol, query->environment, FALSE);
}

static SEXP binding_value_error(SEXP condition, void *data) {
    if (Rf_inherits(condition, "getVarExError")) {
        ((binding_value_query *) data)->missing = 1;
        return R_NilValue;
    }
    /* Do not turn unrelated failures into missing-binding metadata. */
    SEXP stop = R_getVar(Rf_install("stop"), R_BaseEnv, FALSE);
    SEXP call = PROTECT(Rf_lang2(stop, condition));
    Rf_eval(call, R_BaseEnv);
    UNPROTECT(1);
    return R_NilValue;
}

/* Call only after active and unevaluated bindings have been excluded. R's
   public getter rejects a directly missing binding, but successfully returns
   R_MissingArg when that is the cached value of an already-forced promise.
   This works in .GlobalEnv too, without cloning or relying on a promise's
   expression. The probe is needed only for rare missing-valued candidates. */
static int binding_value_is_missing(SEXP environment, SEXP symbol) {
    binding_value_query query = {environment, symbol, 0};
    (void) R_tryCatchError(binding_value_read, &query,
                          binding_value_error, &query);
    return query.missing;
}

/* R 4.5 has no public API for recognizing an unevaluated promise. Use rlang's
   exported, vectorized R API for that step rather than inspecting R internals.
   Active bindings are removed first; env_get_list then sees only values and
   already-forced promises. This also preserves missing arguments without
   evaluating them. No object payloads or environment bindings are copied. */
static SEXP binding_info_compat(SEXP environment, SEXP names) {
    const R_xlen_t length = XLENGTH(names);
    SEXP result = PROTECT(Rf_allocVector(STRSXP, length));
    SEXP missing_kind = PROTECT(Rf_mkChar("missing"));
    SEXP active_kind = PROTECT(Rf_mkChar("active"));
    SEXP value_kind = PROTECT(Rf_mkChar("value"));
    SEXP promise_kind = PROTECT(Rf_mkChar("promise"));
    R_xlen_t remaining = 0;

    for (R_xlen_t i = 0; i < length; ++i) {
        SEXP name = STRING_ELT(names, i);
        SEXP kind = missing_kind;
        if (name != NA_STRING && LENGTH(name) != 0) {
            SEXP symbol = Rf_installChar(name);
            if (R_existsVarInFrame(environment, symbol)) {
                if (R_BindingIsActive(symbol, environment)) kind = active_kind;
                else { kind = value_kind; ++remaining; }
            }
        }
        SET_STRING_ELT(result, i, kind);
    }

    if (remaining) {
        SEXP pending = PROTECT(Rf_allocVector(STRSXP, remaining));
        R_xlen_t *indices = (R_xlen_t *) R_alloc((size_t) remaining, sizeof(R_xlen_t));
        R_xlen_t position = 0;
        for (R_xlen_t i = 0; i < length; ++i) {
            if (STRING_ELT(result, i) == value_kind) {
                SET_STRING_ELT(pending, position, STRING_ELT(names, i));
                indices[position++] = i;
            }
        }

        /* Both functions are exported by rlang and declared as imports. */
        SEXP package = PROTECT(Rf_mkString("rlang"));
        SEXP namespace = PROTECT(R_FindNamespace(package));
        SEXP lazy_function = R_getVar(Rf_install("env_binding_are_lazy"), namespace, FALSE);
        SEXP lazy_call = PROTECT(Rf_lang3(lazy_function, environment, pending));
        SEXP lazy = PROTECT(Rf_eval(lazy_call, R_BaseEnv));
        if (TYPEOF(lazy) != LGLSXP || XLENGTH(lazy) != remaining)
            Rf_error("Invalid lazy-binding metadata from rlang.");

        position = 0;
        for (R_xlen_t i = 0; i < remaining; ++i) {
            int delayed = LOGICAL_ELT(lazy, i);
            if (delayed == NA_LOGICAL)
                Rf_error("Missing lazy-binding metadata from rlang.");
            if (delayed) SET_STRING_ELT(result, indices[i], promise_kind);
            else indices[position++] = indices[i];
        }

        if (position) {
            SEXP ordinary = PROTECT(Rf_allocVector(STRSXP, position));
            for (R_xlen_t i = 0; i < position; ++i)
                SET_STRING_ELT(ordinary, i, STRING_ELT(names, indices[i]));
            SEXP get_function = R_getVar(Rf_install("env_get_list"), namespace, FALSE);
            SEXP get_call = PROTECT(Rf_lang3(get_function, environment, ordinary));
            SEXP values = PROTECT(Rf_eval(get_call, R_BaseEnv));
            if (TYPEOF(values) != VECSXP || XLENGTH(values) != position)
                Rf_error("Invalid value-binding metadata from rlang.");
            for (R_xlen_t i = 0; i < position; ++i) {
                if (VECTOR_ELT(values, i) == R_MissingArg &&
                    binding_value_is_missing(environment,
                        Rf_installChar(STRING_ELT(names, indices[i]))))
                    SET_STRING_ELT(result, indices[i], missing_kind);
            }
            UNPROTECT(3);
        }
        UNPROTECT(5);
    }

    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(5);
    return result;
}
#endif

SEXP C_binding_info(SEXP environment, SEXP names) {
    if (TYPEOF(environment) != ENVSXP || TYPEOF(names) != STRSXP)
        Rf_error("Expected an environment and character names.");
#if R_VERSION < R_Version(4, 6, 0)
    return binding_info_compat(environment, names);
#else
    SEXP result = PROTECT(Rf_allocVector(STRSXP, XLENGTH(names)));
    for (R_xlen_t i = 0; i < XLENGTH(names); ++i) {
        SEXP name = STRING_ELT(names, i);
        const char *kind = "missing";
        if (name != NA_STRING && LENGTH(name) != 0) {
            SEXP symbol = Rf_installChar(name);
            switch (R_GetBindingType(symbol, environment)) {
            case R_BindingTypeValue:
            case R_BindingTypeForced: kind = "value"; break;
            case R_BindingTypeDelayed: kind = "promise"; break;
            case R_BindingTypeActive: kind = "active"; break;
            case R_BindingTypeUnbound:
            case R_BindingTypeMissing: kind = "missing"; break;
            default: kind = "missing"; break;
            }
        }
        SET_STRING_ELT(result, i, Rf_mkChar(kind));
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(1);
    return result;
#endif
}
