; docformat = 'rst'

;+
; Browse through directories containing CoMP data files.
;-


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


;+
; Return the structure representing a file in the table.
;
; :Returns:
;   structure
;-
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


;+
; Handle a directory of date directories which contain FITS files.
;
; :Params:
;   dir : in, required, type=string
;     root directory to load
;-
pro comp_dir_browser::load_directory, dir
  compile_opt strictarr

  child = widget_info(self.tree, /child)
  if (child eq 0L) then begin
    self->set_title, dir
  endif else begin
    self->set_title, 'several directories'
  endelse

  self->set_status, 'Loading ' + dir + '...'

  ; add dir as root of tree
  root = widget_tree(self.tree, value=file_basename(dir), /folder, $
                     uvalue=dir, uname='root')

  ; add subdirs of dir as nodes, uname='datedir'
  datedirs = file_search(filepath('*', root=dir), /test_directory, $
                          count=n_datedirs)
  for d = 0L, n_datedirs - 1L do begin
    ; TODO: identify datedir as containing L0 or L1 data, set icon to
    ; represent
    files = file_search(filepath('*.fts', root=datedirs[d]), $
                        count=n_files, /fold_case)
    datedir = widget_tree(root, $
                          value=file_basename(datedirs[d]) $
                            + ' - ' + strtrim(n_files, 2) + ' files', $
                          uvalue=datedirs[d], uname='datedir')
  endfor

  self->set_status, 'Ready'
end


;+
; Load a date directory.
;
; :Params:
;   datedir : in, required, type=string
;     directory with name of the form YYYYMMDD which contains CoMP
;     data files
;-
pro comp_dir_browser::load_datedir, datedir
  compile_opt strictarr

  self->set_status, 'Loading ' + datedir + '...'

  files = file_search(filepath('*.fts', root=datedir), count=n_files, /fold_case)
  *(self.files) = files

  if (n_files eq 0L) then begin
    widget_control, self.table, ysize=0
  endif else begin
    files_info = replicate(comp_dir_browser_row(), n_files)

    ; TODO: handle L0 vs L1 differently

    for f = 0L, n_files - 1L do begin
      if ((f + 1) mod 10 eq 0) then begin
          self->set_status, string(datedir, f + 1, n_files, $
                                   format='(%"Loading %s: %d/%d...")')
      endif

      ; set time fields
      basename = file_basename(files[f])
      file_tokens = strsplit(basename, '.', /extract, count=n_files_tokens)
      time = file_tokens[1]
      time = strmid(time, 0, 2) $
               + ':' + strmid(time, 2, 2) $
               + ':' + strmid(time, 4, 2)
      files_info[f].time = time

      fits_open, files[f], fcb
      comp_inventory, fcb, beam, group, wave, pol, type, expose, cover, cal_pol, cal_ret

      n = n_elements(pol)
      case type of
        'OPAL': files_info[f].n_flat = strtrim(n, 2)
        'DATA': files_info[f].n_data = strtrim(n, 2)
        'DARK': files_info[f].n_dark = strtrim(n, 2)
      endcase
      files_info[f].exposure = string(expose, format='(%"%0.1f ms")')
      files_info[f].pol_states = strjoin(strtrim(pol[uniq(pol, sort(pol))], 2), ', ')
      files_info[f].wavelengths = strjoin(strtrim(wave[uniq(wave, sort(wave))], 2), ', ')

      fits_close, fcb
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
    'tlb': begin
        tlb_geometry = widget_info(self.tlb, /geometry)
        content_base_geometry = widget_info(widget_info(self.tlb, /child), /geometry)
        tree_geometry = widget_info(self.tree, /geometry)
        table_geometry = widget_info(self.table, /geometry)
        statusbar_geometry = widget_info(self.statusbar, /geometry)

        table_width = event.x - tree_geometry.scr_xsize $
                        - 2 * tlb_geometry.xpad $
                        - content_base_geometry.xpad $
                        - 3
        statusbar_width = table_width + tree_geometry.scr_xsize $
                            + content_base_geometry.xpad
        height = event.y - 3 * tlb_geometry.ypad $
                   - 2 * content_base_geometry.ypad $
                   - statusbar_geometry.scr_ysize $
                   - 2 * statusbar_geometry.margin

        widget_control, self.tlb, update=0
        widget_control, self.tree, scr_ysize=height
        widget_control, self.table, scr_xsize=table_width, scr_ysize=height
        widget_control, self.statusbar, scr_xsize=statusbar_width
        widget_control, self.tlb, update=1
      end
    'table': begin
        case tag_names(event, /structure_name) of
          'WIDGET_TABLE_CELL_SEL': begin
              table_geometry = widget_info(self.table, /geometry)
              if (event.sel_top lt 0 || event.sel_bottom ge table_geometry.ysize) then return
              current_view = widget_info(self.table, /table_view)
              widget_control, self.table, $
                              set_table_select=[0, event.sel_top, $
                                                6, event.sel_bottom]
              widget_control, self.table, set_table_view=current_view
              self.selection = [event.sel_top, event.sel_bottom]
            end
          'WIDGET_CONTEXT': begin
              widget_displaycontextmenu, event.id, event.x, event.y, self.context_base
            end
          else: help, event
        endcase
      end
    'display_files': begin
        if (~obj_valid(self.file_browser)) then begin
          resolve_routine, 'comp_browser'
          self.file_browser = mg_fits_browser(classname='comp_browser')
        endif
        self.file_browser->load_files, (*(self.files))[self.selection[0]:self.selection[1]]
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
; Return the proportional widths of the various columns of the table widget.
;-
function comp_dir_browser_colwidths
  compile_opt strictarr

  colwidths = [0.1, 0.1, 0.08, 0.08, 0.08, 0.2775, 0.2775]
  return, colwidths / total(colwidths) * 0.995
end


;+
; Create the widget hierarchy.
;-
pro comp_dir_browser::create_widgets
  compile_opt strictarr

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb')
  
  ; content row
  content_base = widget_base(self.tlb, /row, xpad=0)

  tree_xsize = 250
  table_xsize = 700
  scr_ysize = 600
  xpad = 0

  self.tree = widget_tree(content_base, uname='browser', $
                          scr_xsize=tree_xsize, scr_ysize=scr_ysize)

  self.table = widget_table(content_base, $
                            /no_row_headers, $
                            column_labels=['Time', $
                                           'Exposure', $
                                           'N dark', $
                                           'N flat', $
                                           'N data', $
                                           'Wavelengths', $
                                           'Pol states'], $
                            column_widths=comp_dir_browser_colwidths() * table_xsize, $
                            xsize=7, $
                            scr_xsize=table_xsize, $
                            scr_ysize=scr_ysize, $
                            uname='table', $
                            /resizeable_columns, $
                            /all_events, $
                            /context_events)

  self.context_base = widget_base(self.table, /context_menu)
  display_button = widget_button(self.context_base, value='Display files', $
                                 uname='display_files')

  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=tree_xsize + table_xsize + xpad, $
                                /align_left, /sunken_frame)
end


;+
; Draw the widget hierarchy.
;-
pro comp_dir_browser::realize_widgets
  compile_opt strictarr

  widget_control, self.table, ysize=0
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

;+
; Free resources of CoMP directory browser object.
;-
pro comp_dir_browser::cleanup
  compile_opt strictarr

  ptr_free, self.files
  obj_destroy, self.file_browser
end


;+
; Create a CoMP directory browser object.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;   directory : in, optional, type=string
;     directory to browse
;   tlb : out, optional, type=long
;     widget identifier of the CoMP directory browser
;-
function comp_dir_browser::init, directory=directory, tlb=tlb
  compile_opt strictarr

  self.title = 'CoMP data directory browser'

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self->set_status, 'Ready'

  tlb = self.tlb
  self.files = ptr_new(/allocate_heap)

  if (n_elements(directory) gt 0L) then self->load_directory, directory

  return, 1
end


;+
; Define the `comp_dir_browser` class.
;-
pro comp_dir_browser__define
  compile_opt strictarr

  define = { comp_dir_browser, $
             date_dir: '', $
             tlb: 0L, $
             tree: 0L, $
             table: 0L, $
             context_base: 0L, $
             statusbar: 0L, $
             title: '', $
             selection: lonarr(2), $
             files: ptr_new(), $
             file_browser: obj_new() $
           }
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
