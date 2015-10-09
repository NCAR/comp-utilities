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
; Thin procedural wrapper to call `::cleanup_widgets` cleanup routine.
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
           pol_states: '', $
           obs_plan: '', $
           obs_id: ''}
end


;+
; Determine if a directory contains raw data (level 0), processed
; (level 1 or 2) data, or no data.
;
; :Returns:
;   -1, 0, 1, or 2
;
; :Params:
;   datedir : in, required, type=string
;     directory to check
;
; :Keywords:
;   files : out, optional, type=strarr
;     FITS files in directory
;   n_files : out, optional, type=long
;     number of FITS files in directory
;-
function comp_dir_browser_findlevel, datedir, files=files, n_files=n_files
  compile_opt strictarr

  files = file_search(filepath('*.fts', root=datedir), /fold_case, count=n_files)
  if (n_files eq 0) then return, -1

  raw_re = '[[:digit:]]{8}\.[[:digit:]]{6}\.FTS'
  n_raw_files = total(stregex(files, raw_re, /boolean), /integer)

  if (n_raw_files eq n_files) then return, 0

  return, 1
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

  dirname = file_basename(file_expand_path(dir))

  child = widget_info(self.tree, /child)
  if (child eq 0L) then begin
    self->set_title, dirname
  endif else begin
    self->set_title, 'several directories'
  endelse

  self->set_status, 'Loading ' + dirname + '...'

  ; add dir as root of tree
  root = widget_tree(self.tree, value=dirname, /folder, $
                     uvalue=file_expand_path(dir), $
                     uname='root', $
                     tooltip=file_expand_path(dir))

  raw_bmp = read_png(filepath('raw.png', root=mg_src_root()))
  raw_bmp = transpose(raw_bmp, [1, 2, 0])

  level1_bmp = read_png(filepath('level1.png', root=mg_src_root()))
  level1_bmp = transpose(level1_bmp, [1, 2, 0])

  ; add subdirs of dir as nodes, uname='datedir'
  datedirs = file_search(filepath('*', root=dir), /test_directory, $
                          count=n_datedirs)
  widget_control, self.tree, update=0
  for d = 0L, n_datedirs - 1L do begin
    ; TODO: identify datedir as containing L0 or L1 data, set icon to
    ; represent
    level = comp_dir_browser_findlevel(datedirs[d], files=files, n_files=n_files)
    case level of
      -1: bitmap = bytarr(16, 16, 3)
       0: bitmap = raw_bmp
       1: bitmap = level1_bmp
       2: bitmap = level1_bmp
    endcase
    datedir = widget_tree(root, $
                          value=file_basename(datedirs[d]) $
                            + ' - ' + strtrim(n_files, 2) + ' files', $
                          bitmap=bitmap, $
                          uvalue=datedirs[d], $
                          uname='datedir', $
                          tooltip=file_expand_path(datedirs[d]))
  endfor
  widget_control, self.tree, update=1

  self->set_status, 'Ready'
end


;+
; Compute total number of dark, flat, and data images.
;
; :Returns:
;   `lonarr(3)`
;
; :Params:
;   files_info : in, optional, type=array of structures
;     array of structures of the same type as the return value of
;     `COMP_DIR_BROWSER_ROW`; returns `lonarr(3)` if not present
;-
function comp_dir_browser::compute_totals, files_info
  compile_opt strictarr

  if (n_elements(files_info) eq 0L) then return, lonarr(3)

  n_dark = total(long(files_info.n_dark), /preserve_type)
  n_flat = total(long(files_info.n_flat), /preserve_type)
  n_data = total(long(files_info.n_data), /preserve_type)

  return, [n_dark, n_flat, n_data]
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

  if (self.inventories->hasKey(datedir)) then begin
    files_info = (self.inventories)[datedir]

    *(self.files) = (self.files_cache)[datedir]
    n_files = n_elements(*(self.files))
  endif else begin
    self->set_status, 'Loading ' + datedir + '...'

    files = file_search(filepath('*.fts', root=datedir), count=n_files, /fold_case)
    *(self.files) = files

    (self.files_cache)[datedir] = n_files eq 0L ? [] : files

    if (n_files eq 0L) then begin
      widget_control, self.table, ysize=0
      file_info = {}
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

        comp_query_file, files[f], $
                         beam_state=beam, wavelength=wave, $
                         polarization=pol, type=type, $
                         exposure=expose, cover=cover, $
                         observation_id=obs_id, observation_plan=obs_plan

        n = n_elements(pol)
        case type of
          'OPAL': files_info[f].n_flat = strtrim(n, 2)
          'DATA': files_info[f].n_data = strtrim(n, 2)
          'DARK': files_info[f].n_dark = strtrim(n, 2)
        endcase

        files_info[f].exposure = string(expose, format='(%"%0.1f ms")')
        files_info[f].pol_states = strjoin(strtrim(pol[uniq(pol, sort(pol))], 2), ', ')
        files_info[f].wavelengths = strjoin(strtrim(wave[uniq(wave, sort(wave))], 2), ', ')
        files_info[f].obs_plan = obs_plan
        files_info[f].obs_id = obs_id
      endfor
    endelse

    (self.inventories)[datedir] = files_info
  endelse

  widget_control, self.table, set_value=files_info, ysize=n_files

  total_images = self->compute_totals(files_info)
  format = '(%"Images loaded: %d dark images, %d flat images, %d data images")'
  self->set_status, string(total_images[0], $
                           total_images[1], $
                           total_images[2], $
                           format=format)
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
        table_column_widths = widget_info(self.table, /column_widths)

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

        ; extra room automatically goes to wavelengths
        table_column_widths[5] += table_width - table_geometry.scr_xsize
        table_column_widths[5] >= 100

        widget_control, self.table, column_widths=table_column_widths

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
                                                8, event.sel_bottom]
              widget_control, self.table, set_table_view=current_view
              self.selection = [event.sel_top, event.sel_bottom]
            end
          'WIDGET_CONTEXT': begin
              widget_displaycontextmenu, event.id, event.x, event.y, self.context_base
            end
          else:
        endcase
      end
    'display_files': begin
        if (~obj_valid(self.file_browser)) then begin
          resolve_routine, 'comp_browser'
          self.file_browser = mg_fits_browser(classname='comp_browser')
        endif
        self.file_browser->load_files, (*(self.files))[self.selection[0]:self.selection[1]]
      end
    'compute_totals': begin
        widget_control, self.table, get_value=files_info
        total_images = self->compute_totals(files_info)
        format = '(%"%d dark images, %d flat images, %d data images")'
        self->set_status, string(total_images[0], $
                                 total_images[1], $
                                 total_images[2], $
                                 format=format)
      end
    'root': begin
        widget_control, event.id, get_uvalue=full_path
        self->set_status, full_path
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

  colwidths = [0.1, 0.1, 0.08, 0.08, 0.08, 0.2775, 0.2775, 0.15, 0.15]
  return, colwidths / total(colwidths) * 0.975
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
  table_xsize = 850
  scr_ysize = 600
  xpad = 0

  self.tree = widget_tree(content_base, uname='browser', $
                          scr_xsize=tree_xsize, scr_ysize=scr_ysize)

  col_titles = ['Time', $
                'Exposure', $
                'N dark', $
                'N flat', $
                'N data', $
                'Wavelengths', $
                'Pol states', $
                'Obs plan', $
                'Obs ID']
  self.table = widget_table(content_base, $
                            /no_row_headers, $
                            column_labels=col_titles, $
                            column_widths=comp_dir_browser_colwidths() * table_xsize, $
                            xsize=n_elements(col_titles), $
                            scr_xsize=table_xsize, $
                            scr_ysize=scr_ysize, $
                            uname='table', $
                            /resizeable_columns, $
                            /all_events, $
                            /context_events)

  self.context_base = widget_base(self.table, /context_menu)
  display_button = widget_button(self.context_base, value='Display files', $
                                 uname='display_files')
  compute_button = widget_button(self.context_base, value='Compute totals', $
                                 uname='compute_totals')

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
  obj_destroy, [self.file_browser, self.inventories, self.files_cache]
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

  self.inventories = hash()
  self.files_cache = hash()

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
             file_browser: obj_new(), $
             inventories: obj_new(), $
             files_cache: obj_new() $
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
  on_error, 2
  common comp_dir_browser, browser

  _dir = n_elements(pdirectory) gt 0L ? pdirectory : (n_elements(kdirectory) gt 0L ? kdirectory : '')
  if (_dir eq '') then message, 'directory not specified'

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
