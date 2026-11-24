#include "keyparse.h"
#include <limits.h>

const char *prism_key_name(enum prism_key key) {
    static const char *names[] = {
        "", "unknown", "escape", "enter", "tab", "backspace",
        "delete", "insert", "left", "right", "up", "down",
        "home", "end", "pageup", "pagedown", "f1", "f2", "f3", "f4",
        "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12",
        "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20",
        "f21", "f22", "f23", "f24",
        "", "paste_start", "paste_end"
    };
    if (key < PK_NONE || key > PK_PASTE_END) return "unknown";
    return names[(int)key];
}

static int modifiers(unsigned int parameter) {
    unsigned int bits = parameter > 0 ? parameter - 1 : 0;
    return ((bits & 4U) ? PM_CTRL : 0) |
           ((bits & 2U) ? PM_ALT : 0) |
           ((bits & 1U) ? PM_SHIFT : 0);
}

prism_key prism_parse_control(unsigned int value) {
    prism_key result = {PK_CHARACTER, value, 0};
    if (value == 27U) result.key = PK_ESCAPE;
    else if (value == 13U) result.key = PK_ENTER;
    else if (value == 9U) result.key = PK_TAB;
    else if (value == 127U || value == 8U) result.key = PK_BACKSPACE;
    else if (value < 32U) {
        result.modifiers = PM_CTRL;
        result.codepoint = value == 0U ? 32U : value <= 26U ? value + 96U : value + 64U;
    }
    return result;
}

prism_mouse prism_parse_mouse(const unsigned char *bytes, size_t length) {
    prism_mouse result = {0, 0, 0, PB_NONE, PA_NONE, 0};
    unsigned int parameters[3] = {0, 0, 0};
    size_t parameter = 0;
    int digits = 0;
    if (length < 9 || bytes[0] != 27 || bytes[1] != '[' || bytes[2] != '<' ||
        (bytes[length - 1] != 'M' && bytes[length - 1] != 'm')) return result;
    for (size_t i = 3; i + 1 < length; ++i) {
        unsigned char byte = bytes[i];
        if (byte >= '0' && byte <= '9') {
            unsigned int digit = byte - '0';
            if (parameters[parameter] > ((unsigned int) INT_MAX - digit) / 10U)
                return result;
            parameters[parameter] = parameters[parameter] * 10U + digit;
            digits = 1;
        } else if (byte == ';' && parameter < 2 && digits) {
            ++parameter;
            digits = 0;
        } else return result;
    }
    if (parameter != 2 || !digits || parameters[0] > 127U ||
        parameters[1] == 0 || parameters[2] == 0) return result;
    unsigned int code = parameters[0];
    unsigned int button = code & 3U;
    result.x = (int) parameters[1];
    result.y = (int) parameters[2];
    result.modifiers = ((code & 4U) ? PM_SHIFT : 0) |
        ((code & 8U) ? PM_ALT : 0) | ((code & 16U) ? PM_CTRL : 0);
    if (code & 64U) {
        if (button > 1U || bytes[length - 1] == 'm' || (code & 32U)) return result;
        result.button = button == 0U ? PB_UP : PB_DOWN;
        result.action = PA_WHEEL;
    } else {
        result.button = button == 0U ? PB_LEFT : button == 1U ? PB_MIDDLE :
            button == 2U ? PB_RIGHT : PB_NONE;
        result.action = bytes[length - 1] == 'm' ? PA_RELEASE :
            (code & 32U) ? PA_MOVE : PA_PRESS;
        if (result.button == PB_NONE && result.action == PA_PRESS) return result;
    }
    result.valid = 1;
    return result;
}

prism_key prism_parse_escape(const unsigned char *bytes, size_t length) {
    prism_key result = {PK_UNKNOWN, 0U, 0};
    unsigned int parameters[8] = {0};
    size_t count = 1, i, offset;
    unsigned char final;
    if (length < 2 || bytes[0] != 27) return result;
    if (bytes[1] != '[' && bytes[1] != 'O') {
        unsigned int value;
        size_t consumed;
        if (prism_utf8_decode(bytes + 1, length - 1, &value, &consumed) != 1 ||
            consumed != length - 1) return result;
        result = prism_parse_control(value);
        result.modifiers |= PM_ALT;
        if (value >= 'A' && value <= 'Z') {
            result.codepoint = value + 32U;
            result.modifiers |= PM_SHIFT;
        }
        return result;
    }
    if (length < 3) return result;
    final = bytes[length - 1];
    offset = 2;
    for (i = offset; i + 1 < length; ++i) {
        unsigned char c = bytes[i];
        if (c >= '0' && c <= '9') {
            if (parameters[count - 1] > (UINT_MAX - 9U) / 10U) return result;
            parameters[count - 1] = 10U * parameters[count - 1] + (c - '0');
        } else if (c == ';') {
            if (count >= 8) return result;
            ++count;
        } else if (c == ':') {
            /* Kitty subparameters carry alternate keys or event types. */
            while (i + 1 < length - 1 && bytes[i + 1] != ';') ++i;
        } else return result;
    }
    if (count >= 2) result.modifiers = modifiers(parameters[1]);
    switch (final) {
    case 'A': result.key = PK_UP; break;
    case 'B': result.key = PK_DOWN; break;
    case 'C': result.key = PK_RIGHT; break;
    case 'D': result.key = PK_LEFT; break;
    case 'H': result.key = PK_HOME; break;
    case 'F': result.key = PK_END; break;
    case 'P': result.key = PK_F1; break;
    case 'Q': result.key = PK_F2; break;
    case 'R': result.key = PK_F3; break;
    case 'S': result.key = PK_F4; break;
    case 'Z': result.key = PK_TAB; result.modifiers |= PM_SHIFT; break;
    case 'u':
        if (parameters[0] >= 57376U && parameters[0] <= 57387U)
            result.key = (enum prism_key) (PK_F13 + (parameters[0] - 57376U));
        else {
            result.key = PK_CHARACTER;
            result.codepoint = parameters[0];
        }
        break;
    case '~':
        switch (parameters[0]) {
        case 1: case 7: result.key = PK_HOME; break;
        case 2: result.key = PK_INSERT; break;
        case 3: result.key = PK_DELETE; break;
        case 4: case 8: result.key = PK_END; break;
        case 5: result.key = PK_PAGEUP; break;
        case 6: result.key = PK_PAGEDOWN; break;
        case 11: result.key = PK_F1; break;
        case 12: result.key = PK_F2; break;
        case 13: result.key = PK_F3; break;
        case 14: result.key = PK_F4; break;
        case 15: result.key = PK_F5; break;
        case 17: result.key = PK_F6; break;
        case 18: result.key = PK_F7; break;
        case 19: result.key = PK_F8; break;
        case 20: result.key = PK_F9; break;
        case 21: result.key = PK_F10; break;
        case 23: result.key = PK_F11; break;
        case 24: result.key = PK_F12; break;
        case 25: result.key = PK_F13; break;
        case 26: result.key = PK_F14; break;
        case 28: result.key = PK_F15; break;
        case 29: result.key = PK_F16; break;
        case 31: result.key = PK_F17; break;
        case 32: result.key = PK_F18; break;
        case 33: result.key = PK_F19; break;
        case 34: result.key = PK_F20; break;
        case 27:
            if (count == 3) {
                result.key = PK_CHARACTER;
                result.codepoint = parameters[2];
            }
            break;
        case 200: result.key = PK_PASTE_START; break;
        case 201: result.key = PK_PASTE_END; break;
        default: break;
        }
        break;
    default: break;
    }
    if (result.key == PK_CHARACTER) {
        unsigned int value = result.codepoint;
        int saved = result.modifiers;
        if (value == 27U || value == 13U || value == 10U ||
            value == 9U || value == 127U || value == 8U || value < 32U) {
            result = prism_parse_control(value);
            result.modifiers |= saved;
        } else if (value >= 'A' && value <= 'Z' && saved != 0) {
            result.codepoint = value + 32U;
        }
        if (value > 0x10ffffU || (value >= 0xd800U && value <= 0xdfffU)) {
            result.key = PK_UNKNOWN;
            result.codepoint = 0;
        }
    }
    return result;
}

size_t prism_utf8_encode(unsigned int value, unsigned char result[4]) {
    if (value > 0x10ffffU || (value >= 0xd800U && value <= 0xdfffU)) value = 0xfffdU;
    if (value <= 0x7fU) { result[0] = (unsigned char)value; return 1; }
    if (value <= 0x7ffU) {
        result[0] = (unsigned char)(0xc0U | (value >> 6));
        result[1] = (unsigned char)(0x80U | (value & 0x3fU));
        return 2;
    }
    if (value <= 0xffffU) {
        result[0] = (unsigned char)(0xe0U | (value >> 12));
        result[1] = (unsigned char)(0x80U | ((value >> 6) & 0x3fU));
        result[2] = (unsigned char)(0x80U | (value & 0x3fU));
        return 3;
    }
    result[0] = (unsigned char)(0xf0U | (value >> 18));
    result[1] = (unsigned char)(0x80U | ((value >> 12) & 0x3fU));
    result[2] = (unsigned char)(0x80U | ((value >> 6) & 0x3fU));
    result[3] = (unsigned char)(0x80U | (value & 0x3fU));
    return 4;
}

int prism_utf8_decode(const unsigned char *bytes, size_t length,
                      unsigned int *value, size_t *consumed) {
    size_t needed, i;
    unsigned int scalar, minimum;
    *consumed = length ? 1 : 0;
    *value = 0xfffdU;
    if (!length) return 0;
    if (bytes[0] < 0x80U) { *value = bytes[0]; return 1; }
    if (bytes[0] >= 0xc2U && bytes[0] <= 0xdfU) {
        needed = 2; scalar = bytes[0] & 0x1fU; minimum = 0x80U;
    } else if (bytes[0] >= 0xe0U && bytes[0] <= 0xefU) {
        needed = 3; scalar = bytes[0] & 0x0fU; minimum = 0x800U;
    } else if (bytes[0] >= 0xf0U && bytes[0] <= 0xf4U) {
        needed = 4; scalar = bytes[0] & 0x07U; minimum = 0x10000U;
    } else return -1;
    for (i = 1; i < needed && i < length; ++i) {
        if ((bytes[i] & 0xc0U) != 0x80U) return -1;
        scalar = (scalar << 6) | (bytes[i] & 0x3fU);
    }
    if (length < needed) return 0;
    if (scalar < minimum || scalar > 0x10ffffU ||
        (scalar >= 0xd800U && scalar <= 0xdfffU)) return -1;
    *consumed = needed;
    *value = scalar;
    return 1;
}

size_t prism_utf8_sanitize(const unsigned char *source, size_t length,
                           unsigned char *target, size_t capacity) {
    size_t i = 0, out = 0;
    while (i < length) {
        unsigned int value;
        unsigned char encoded[4];
        size_t consumed, count;
        int valid = prism_utf8_decode(source + i, length - i, &value, &consumed);
        if (valid != 1 || value == 0) value = 0xfffdU;
        if (valid != 1) consumed = 1;
        count = prism_utf8_encode(value, encoded);
        if (out > capacity || count > capacity - out) break;
        for (size_t j = 0; j < count; ++j) target[out++] = encoded[j];
        i += consumed;
    }
    return out;
}
