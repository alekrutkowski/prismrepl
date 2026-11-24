#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern SEXP C_term_open(SEXP, SEXP);
extern SEXP C_term_close(SEXP);
extern SEXP C_term_read(SEXP, SEXP);
extern SEXP C_term_reset(void);
extern SEXP C_term_size(void);
extern SEXP C_term_capabilities(void);
extern SEXP C_term_output_open(SEXP);
extern SEXP C_term_output_close(SEXP);
extern SEXP C_term_output_reset(void);
extern SEXP C_term_output_state(void);
extern SEXP C_term_output_screen(SEXP);
extern SEXP C_term_write(SEXP, SEXP, SEXP);
extern SEXP C_parse_status(SEXP);
extern SEXP C_binding_info(SEXP, SEXP);
extern SEXP C_clipboard_read(SEXP);
extern SEXP C_clipboard_write(SEXP);

static const R_CallMethodDef call_methods[] = {
    {"C_term_open", (DL_FUNC) &C_term_open, 2},
    {"C_term_close", (DL_FUNC) &C_term_close, 1},
    {"C_term_read", (DL_FUNC) &C_term_read, 2},
    {"C_term_reset", (DL_FUNC) &C_term_reset, 0},
    {"C_term_size", (DL_FUNC) &C_term_size, 0},
    {"C_term_capabilities", (DL_FUNC) &C_term_capabilities, 0},
    {"C_term_output_open", (DL_FUNC) &C_term_output_open, 1},
    {"C_term_output_close", (DL_FUNC) &C_term_output_close, 1},
    {"C_term_output_reset", (DL_FUNC) &C_term_output_reset, 0},
    {"C_term_output_state", (DL_FUNC) &C_term_output_state, 0},
    {"C_term_output_screen", (DL_FUNC) &C_term_output_screen, 1},
    {"C_term_write", (DL_FUNC) &C_term_write, 3},
    {"C_parse_status", (DL_FUNC) &C_parse_status, 1},
    {"C_binding_info", (DL_FUNC) &C_binding_info, 2},
    {"C_clipboard_read", (DL_FUNC) &C_clipboard_read, 1},
    {"C_clipboard_write", (DL_FUNC) &C_clipboard_write, 1},
    {NULL, NULL, 0}
};

void attribute_visible R_init_prismrepl(DllInfo *info) {
    R_registerRoutines(info, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}

void attribute_visible R_unload_prismrepl(DllInfo *info) {
    (void) info;
    (void) C_term_reset();
    (void) C_term_output_reset();
}
