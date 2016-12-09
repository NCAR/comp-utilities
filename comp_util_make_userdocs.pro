; docformat = 'rst'

;+
; Use IDLdoc to create API documentation for CoMP utilities.
;-
pro comp_util_make_userdocs
  compile_opt strictarr

  user = 1B

  args = command_line_args(count=nargs)
  root = nargs gt 1L ? args[0] : mg_src_root()   ; location of this file

  ; browser routines need to be compile beforehand because the class
  ; definitions inside of them are not in a __define.pro file
  resolve_routine, 'comp_browser'
  resolve_routine, 'mg_fits_browser', /is_function

  idldoc, root=filepath('src', root=root), $
          output=filepath(keyword_set(user) $
                            ? 'api-userdocs' $
                            : 'api-docs', $
                          root='.'), $
          title='CoMP utilities API documentation', $
          subtitle='NCAR-HAO Mauna Loa Solar Observatory', $
          /embed, $
          statistics=keyword_set(user) eq 0, $
          /use_latex, $
          format_style='rst', $
          overview=filepath('overview.txt', root=root), $
          user=user
end
