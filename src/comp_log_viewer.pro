; docformat = 'rst'


;+
; Browse through logs from CoMP output.
;-


;= helper routines

;+
; Thin procedural wrapper to call `::handle_events` event handler.
;
; :Params:
;   event : in, required, type=structure
;     event structure for event handler to handle
;-
pro comp_log_viewer_handleevents, event
  compile_opt strictarr

  widget_control, event.top, get_uvalue=browser
  browser->handle_events, event
end


;+
; Thin procedural wrapper to call `::cleanup_widgets` cleanup routine.
;
; :Params:
;   tlb : in, required, type=long
;     top-level base widget identifier
;-
pro comp_log_viewer_cleanup, tlb
  compile_opt strictarr

  widget_control, tlb, get_uvalue=browser
  browser->cleanup_widgets
end


;+
; Launch a `COMP_DIR_BROWSER`.
;
; :Params:
;   pdirectory : in, optional, type=string
;     directory to browse
;
; :Keywords:
;   directory : in, optional, type=string
;     directory to browse
;-
pro comp_log_viewer, pdirectory, directory=kdirectory
  compile_opt strictarr
  on_error, 2
  common comp_log_viewer, browser

  _dir = n_elements(pdirectory) gt 0L ? pdirectory : (n_elements(kdirectory) gt 0L ? kdirectory : '')
  if (_dir eq '') then message, 'directory not specified'

  if (obj_valid(browser)) then begin
    browser->load_directory, _dir
  endif else begin
    browser = obj_new('comp_log_viewer', directory=_dir)
  endelse
end
