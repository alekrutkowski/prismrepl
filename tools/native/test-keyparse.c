#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include "keyparse.h"

static prism_key parse(const char *input) {
    return prism_parse_escape((const unsigned char *)input, strlen(input));
}
static prism_mouse mouse(const char *input) {
    return prism_parse_mouse((const unsigned char *)input, strlen(input));
}
int main(void) {
    prism_key key;
    assert(parse("\033[A").key == PK_UP);
    assert(parse("\033OB").key == PK_DOWN);
    assert(parse("\033[1;5C").key == PK_RIGHT);
    assert(parse("\033[1;5C").modifiers == PM_CTRL);
    assert(parse("\033[Z").key == PK_TAB);
    assert(parse("\033[Z").modifiers == PM_SHIFT);
    assert(parse("\033[200~").key == PK_PASTE_START);
    assert(parse("\033[201~").key == PK_PASTE_END);
    assert(parse("\033[25~").key == PK_F13);
    assert(parse("\033[34~").key == PK_F20);
    assert(parse("\033[57376u").key == PK_F13);
    assert(parse("\033[57387;5u").key == PK_F24);
    assert(parse("\033[57387;5u").modifiers == PM_CTRL);
    assert(strcmp(prism_key_name(PK_F24), "f24") == 0);
    key = parse("\033[109;6u");
    assert(key.key == PK_CHARACTER && key.codepoint == 'm' && key.modifiers == (PM_CTRL | PM_SHIFT));
    key = parse("\033[27;6;77~");
    assert(key.key == PK_CHARACTER && key.codepoint == 'm' && key.modifiers == (PM_CTRL | PM_SHIFT));
    key = parse("\033m");
    assert(key.key == PK_CHARACTER && key.codepoint == 'm' && key.modifiers == PM_ALT);
    key = parse("\033\r");
    assert(key.key == PK_ENTER && key.modifiers == PM_ALT);
    assert(parse("\033[999999999999999999999999u").key == PK_UNKNOWN);
    assert(parse("\033[55296u").key == PK_UNKNOWN);
    assert(parse("\033[1114112u").key == PK_UNKNOWN);
    assert(prism_parse_control(3).modifiers == PM_CTRL);
    assert(prism_parse_control(3).codepoint == 'c');
    assert(prism_parse_control(13).key == PK_ENTER);
    assert(prism_parse_control(10).key == PK_CHARACTER);
    assert(prism_parse_control(10).codepoint == 'j');
    assert(prism_parse_control(10).modifiers == PM_CTRL);
    key = parse("\033[10u");
    assert(key.key == PK_CHARACTER && key.codepoint == 'j' && key.modifiers == PM_CTRL);
    assert(prism_parse_control(127).key == PK_BACKSPACE);
    assert(prism_parse_control(0).codepoint == ' ');
    assert(prism_parse_control(28).codepoint == '\\');
    assert(prism_parse_control(29).codepoint == ']');
    assert(prism_parse_control(30).codepoint == '^');
    assert(prism_parse_control(31).codepoint == '_');
    key = parse("\033[61;5u");
    assert(key.codepoint == '=' && key.modifiers == PM_CTRL);
    key = parse("\033[43;3u");
    assert(key.codepoint == '+' && key.modifiers == PM_ALT);
    prism_mouse event = mouse("\033[<0;12;8M");
    assert(event.valid && event.button == PB_LEFT && event.action == PA_PRESS);
    assert(event.x == 12 && event.y == 8 && event.modifiers == 0);
    event = mouse("\033[<0;12;8m");
    assert(event.valid && event.button == PB_LEFT && event.action == PA_RELEASE);
    event = mouse("\033[<1;3;4M");
    assert(event.valid && event.button == PB_MIDDLE);
    event = mouse("\033[<30;300;400M");
    assert(event.valid && event.button == PB_RIGHT && event.x == 300 && event.y == 400);
    assert(event.modifiers == (PM_CTRL | PM_ALT | PM_SHIFT));
    event = mouse("\033[<32;2;3M");
    assert(event.valid && event.action == PA_MOVE && event.button == PB_LEFT);
    event = mouse("\033[<48;20;13M");
    assert(event.valid && event.action == PA_MOVE && event.button == PB_LEFT);
    assert(event.modifiers == PM_CTRL && event.x == 20 && event.y == 13);
    event = mouse("\033[<64;12;8M");
    assert(event.valid && event.button == PB_UP && event.action == PA_WHEEL);
    event = mouse("\033[<65;2147483647;1M");
    assert(event.valid && event.button == PB_DOWN && event.x == 2147483647);
    const char *invalid_mouse[] = {"\033[<0;0;1M", "\033[<0;1;0M", "\033[<0;;1M",
        "\033[<;1;1M", "\033[<0;1;1;M", "\033[<0;1;1", "\033[<0;2147483648;1M",
        "\033[<999999999999999999;1;1M", "\033[<64;1;1m", "\033[<3;1;1M",
        "\033[<128;1;1M", "\033[<66;1;1M", "\033[<0;-1;1M", "\033[<0;1;1X"};
    for (size_t i = 0; i < sizeof(invalid_mouse) / sizeof(invalid_mouse[0]); ++i)
        assert(!mouse(invalid_mouse[i]).valid);
    for (unsigned int cp=1; cp <= 0x10ffff; ++cp) {
        if (cp >= 0xd800 && cp <= 0xdfff) continue;
        unsigned char bytes[4]; unsigned int result; size_t consumed;
        size_t length=prism_utf8_encode(cp,bytes);
        assert(prism_utf8_decode(bytes,length,&result,&consumed)==1);
        assert(result==cp && consumed==length);
    }
    uint32_t seed=47;
    for (int run=0;run<100000;++run) {
        unsigned char input[97], output[300];
        size_t length=(unsigned)run%sizeof(input);
        for(size_t j=0;j<length;++j){seed=seed*1664525U+1013904223U; input[j]=(unsigned char)(seed>>24);}
        if(length) input[0]=27;
        (void)prism_parse_escape(input,length);
        (void)prism_parse_mouse(input,length);
        size_t written=prism_utf8_sanitize(input,length,output,sizeof(output));
        size_t offset=0;
        while(offset<written){unsigned int cp;size_t consumed;assert(prism_utf8_decode(output+offset,written-offset,&cp,&consumed)==1);assert(cp!=0);offset+=consumed;}
        (void)prism_utf8_sanitize(input,length,output,run%7);
    }
    puts("PASS: key and mouse protocols, all Unicode scalar values, 100000 malformed input cases");
}
