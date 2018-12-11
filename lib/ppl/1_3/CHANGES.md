# Patched PPL interface files for version 1.3

This directory contain patched versions of PPL interface files:

```
interfaces/interfaced_boxes.hh
interfaces/Prolog/Ciao/ciao_cfli.hh
interfaces/Prolog/Ciao/ciao_efli.cc
interfaces/Prolog/Ciao/ciao_efli.hh
interfaces/Prolog/Ciao/ppl_prolog_sysdep.hh
interfaces/Prolog/ppl_prolog_common.cc
interfaces/Prolog/ppl_prolog_common_defs.hh
interfaces/Prolog/ppl_prolog_common_inlines.hh
```

The rest of files are (patched) versions of files generated
automatically from M4 macros by PPL.

Summary of changes:

  - `ciao_integer` ==> `ciao_mk_c_long or ciao_mk_c_int`
  - `ciao_implicit_state` ==> `ciao_implicit_ctx`
  - `ciao_fits_in_int` ==> `ciao_fits_in_c_long`
  - `ciao_to_integer` ==> `ciao_get_c_long`
  - Added
```
      inline int Prolog_put_nil(Prolog_term_ref& t) {  t = ciao_empty_list();  return 1; }
      inline int Prolog_get_nil(Prolog_term_ref t) { return ciao_is_empty_list(t); }
```

