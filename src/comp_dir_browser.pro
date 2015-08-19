; docformat = 'rst'


;= helper routines

;+
; Thin procedural wrapper to call `::handle_events` event handler.
;
; :Params:
;   event : in, required, type=structure                                                    
;     event structure for event handler to handle                                          
;-
pro comp_dir_browser_handleevents, event
  compile_opt strictarr

  widget_control, event.top, get_uvalue=browser
  browser->handle_events, event
end


;+
; Thin procedural wrapper to call `::cleanup_widgets` cleanup
; routine.
;
; :Params:
;   tlb : in, required, type=long
;     top-level base widget identifier
;-
pro comp_dir_browser_cleanup, tlb
  compile_opt strictarr

  widget_control, tlb, get_uvalue=browser
  browser->cleanup_widgets
end


function comp_dir_browser_row
  compile_opt strictarr

  return, {time: '', $
           exposure: '', $
           n_dark: '', $
           n_flat: '', $
           n_data: '', $
           wavelengths: '', $
           pol_states: ''}
end


;= API

;+
; Set the window title based on the current filename. Set the filename
; to the empty string if there is no title to display.
;
; :Params:
;   filename : in, required, type=string                                                    
;       filename to display in title
;-
pro comp_dir_browser::set_title, filename
  compile_opt strictarr

  title = string(self.title, $
                 filename eq '' ? '' : ' - ', $
                 filename, $
                 format='(%"%s%s%s")')
  widget_control, self.tlb, base_set_title=title
end


;+
; Set the text in the status bar.
;
; :Params:
;   msg : in, optional, type=string
;     message to display in the status bar
;-
pro comp_dir_browser::set_status, msg, clear=clear
  compile_opt strictarr

  _msg = keyword_set(clear) || n_elements(msg) eq 0L ? '' : msg
  widget_control, self.statusbar, set_value=_msg
end


pro comp_dir_browser::load_directory, dir
  compile_opt strictarr

  self->set_title, dir

  ; add dir as root of tree
  root = widget_tree(self.tree, value=file_basename(dir), /folder, $
                     uvalue=dir, uname='root')

  ; add subdirs of dir as nodes, uname='datedir'
  datedirs = file_search(filepath('*', root=dir), /test_directory, $
                          count=n_datedirs)
  for d = 0L, n_datedirs - 1L do begin
    ; TODO: identify datedir as containing L0 or L1 data, set icon to represent
    datedir = widget_tree(root, value=file_basename(datedirs[d]), $
                          uvalue=datedirs[d], uname='datedir')
  endfor
end


pro comp_dir_browser::load_datedir, datedir
  compile_opt strictarr

  self->set_status, 'Loading ' + datedir + '...'

  files = file_search(filepath('*.fts', root=datedir), count=n_files, /fold_case)
  if (n_files eq 0L) then begin
    widget_control, self.table, ysize=0
  endif else begin
    files_info = replicate(comp_dir_browser_row(), n_files)

    for f = 0L, n_files - 1L do begin
      basename = file_basename(files[f])
      file_tokens = strsplit(basename, '.', /extract, count=n_files_tokens)
      time = file_tokens[1]
      time = strmid(time, 0, 2) $
               + ':' + strmid(time, 2, 2) $
               + ':' + strmid(time, 4, 2)
      files_info[f].time = time

      ; TODO: set files info for no. darks, flats, and data,
      ; wavelengths, polarization states, exposure, etc.

    endfor

    widget_control, self.table, set_value=files_info, ysize=n_files
  endelse

  self->set_status, 'Ready'
end


;= handle events

;+
; Handle all events from the widget program.
;
; :Params:
;   event : in, required, type=structure
;     event structure for event handler to handle
;-
pro comp_dir_browser::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'table': begin
        if (event.type eq 4) then begin   ; selection event
          if (event.sel_left lt 0) then return
          ; TODO: add event.sel_top...event.sel_bottom to comp_browser
        endif
      end
    'datedir': begin
        widget_control, event.id, get_uvalue=datedir
        self->load_datedir, datedir
      end
    else:
  endcase
end


;= widget lifecycle methods

;+
; Handle cleanup when the widget program is destroyed.
;-
pro comp_dir_browser::cleanup_widgets
  compile_opt strictarr

  obj_destroy, self
end


;+
; Create the widget hierarchy.
;-
pro comp_dir_browser::create_widgets
  compile_opt strictarr

  self.tlb = widget_base(title=self.title, /column, uvalue=self, uname='tlb')
  
  ; content row
  content_base = widget_base(self.tlb, /row)

  tree_xsize = 150
  table_xsize = 700
  scr_ysize = 500
  xpad = 4

  ; tree
  self.tree = widget_tree(content_base, uname='browser', $
                          scr_xsize=tree_xsize, scr_ysize=scr_ysize)

  ; table
  self.table = widget_table(content_base, $
                            /no_row_headers, $
                            column_labels=['Time', $
                                           'Exposure', $
                                           'N dark', $
                                           'N flat', $
                                           'N data', $
                                           'Wavelengths', $
                                           'Pol states'], $
                            column_widths=[0.1, $
                                           0.1, $
                                           0.08, $
                                           0.08, $
                                           0.08, $
                                           0.2775, $
                                           0.2775] * table_xsize, $
                            xsize=7, $
                            scr_xsize=table_xsize, $
                            scr_ysize=scr_ysize, $
                            uname='table', $
                            /resizeable_columns, $
                            /all_events)
  widget_control, self.table, ysize=0
  ; status bar
  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=tree_xsize + table_xsize + xpad, $
                                /align_left, /sunken_frame)
end


;+
; Draw the widget hierarchy.
;-
pro comp_dir_browser::realize_widgets
  compile_opt strictarr

  widget_control, self.tlb, /realize
end


;+
; Start `XMANAGER`.
;-
pro comp_dir_browser::start_xmanager
  compile_opt strictarr

  xmanager, 'comp_dir_browser', self.tlb, /no_block, $
            event_handler='comp_dir_browser_handleevents', $
            cleanup='comp_dir_browser_cleanup'
end


;= lifecycle methods

function comp_dir_browser::init, directory=directory, tlb=tlb
  compile_opt strictarr

  self.title = 'FITS Directory Browser'

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self->set_status, 'Ready'
  tlb = self.tlb

  if (n_elements(directory) gt 0L) then self->load_directory, directory

  return, 1
end


pro comp_dir_browser__define
  compile_opt strictarr

  define = { comp_dir_browser, $
             date_dir: '', $
             tlb: 0L, $
             tree: 0L, $
             table: 0L, $
             statusbar: 0L, $
             title: '', $
             file_browser: obj_new() $
           }
end


pro comp_dir_browser, pdirectory, directory=kdirectory
  compile_opt strictarr
  common comp_dir_browser, browser

  _dir = n_elements(pdirectory) gt 0L ? pdirectory : kdirectory

  if (obj_valid(browser)) then begin
    browser->load_directory, _dir
  endif else begin
    browser = obj_new('comp_dir_browser', directory=_dir)
  endelse
end


; main-level example program

comp_dir_browser, '/hao/mlsodata1/Data/CoMP/raw'
comp_dir_browser, '/hao/kaula1/Data/CoMP/process'

end
