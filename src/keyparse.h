#ifndef PRISMREPL_KEYPARSE_H
#define PRISMREPL_KEYPARSE_H

#include <stddef.h>

/* This parser is independent of R and the operating system. */
enum prism_key {
    PK_NONE, PK_UNKNOWN, PK_ESCAPE, PK_ENTER, PK_TAB, PK_BACKSPACE,
    PK_DELETE, PK_INSERT, PK_LEFT, PK_RIGHT, PK_UP, PK_DOWN,
    PK_HOME, PK_END, PK_PAGEUP, PK_PAGEDOWN,
    PK_F1, PK_F2, PK_F3, PK_F4, PK_F5, PK_F6,
    PK_F7, PK_F8, PK_F9, PK_F10, PK_F11, PK_F12,
    PK_F13, PK_F14, PK_F15, PK_F16, PK_F17, PK_F18,
    PK_F19, PK_F20, PK_F21, PK_F22, PK_F23, PK_F24,
    PK_CHARACTER, PK_PASTE_START, PK_PASTE_END
};

#define PM_CTRL 1
#define PM_ALT 2
#define PM_SHIFT 4

typedef struct {
    enum prism_key key;
    unsigned int codepoint;
    int modifiers;
} prism_key;

enum prism_mouse_button { PB_NONE, PB_LEFT, PB_MIDDLE, PB_RIGHT, PB_UP, PB_DOWN };
enum prism_mouse_action { PA_NONE, PA_PRESS, PA_RELEASE, PA_MOVE, PA_WHEEL };

typedef struct {
    int valid;
    int x, y;
    enum prism_mouse_button button;
    enum prism_mouse_action action;
    int modifiers;
} prism_mouse;

const char *prism_key_name(enum prism_key key);
prism_key prism_parse_escape(const unsigned char *bytes, size_t length);
prism_key prism_parse_control(unsigned int value);
prism_mouse prism_parse_mouse(const unsigned char *bytes, size_t length);
size_t prism_utf8_encode(unsigned int value, unsigned char result[4]);
int prism_utf8_decode(const unsigned char *bytes, size_t length,
                      unsigned int *value, size_t *consumed);
size_t prism_utf8_sanitize(const unsigned char *source, size_t length,
                           unsigned char *target, size_t capacity);

#endif
