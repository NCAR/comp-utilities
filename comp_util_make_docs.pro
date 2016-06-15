; docformat = 'rst'

;+
; Use IDLdoc to create API documentation for CoMP utilities.
;
; :Keywords:
;   user : in, optional, type=boolean
;     set to generate user-level documentation
;-
pro comp_util_make_docs, user=user
  compile_opt strictarr

  ; browser routines need to be compile beforehand because the class
  ; definitions inside of them are not in a __define.pro file
  resolve_routine, 'comp_browser'
  resolve_routine, 'mg_fits_browser', /is_function

  root = mg_src_root()
  idldoc, root=filepath('src', root=root), $
          output=filepath(keyword_set(user) $
                            ? 'api-userdocs' $
                            : 'api-docs', $
                          root=root), $
          title='CoMP utilities API documentation', $
          subtitle='NCAR-HAO Mauna Loa Solar Observatory', $
          /embed, $
          statistics=keyword_set(user) eq 0, $
          /use_latex, $
          format_style='rst', $
          overview='overview.txt', $
          user=user
end
