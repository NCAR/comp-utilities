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
function comp_dir_browser::comp_dir_browser_row
  compile_opt strictarr

  return, self.calibration $
            ? {type: '', $
               time: '', $
               exposure: '', $
               n_images: '', $
               wavelengths: '', $
               pol_states: '', $
               obs_plan: '', $
               obs_id: '', $
               polarizer: '', $
               retarder: '', $
               pol_angle: ''} $
            : {type: '', $
               time: '', $
               exposure: '', $
               n_images: '', $
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

  files = file_search(datedir, '*.fts*', $
                      /fold_case, $
                      count=n_files)
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
;   dirs : in, required, type=string/strarr
;     root directory/directories to load
;
; :Keywords:
;   filter : in, optional, type=string, default='*'
;     filter on date directory names to show
;   directory_id : in, optional, type=long
;     if present, use this widget ID as the tree widget node for the directory
;-
pro comp_dir_browser::load_directory, dirs, filter=filter, directory_id=directory_id
  compile_opt strictarr
  on_error, 2

  _filter = n_elements(filter) eq 0L ? '*' : filter

  t0 = systime(/seconds)

  raw_bmp = read_png(filepath('raw.png', root=mg_src_root()))
  raw_bmp = transpose(raw_bmp, [1, 2, 0])

  level1_bmp = read_png(filepath('level1.png', root=mg_src_root()))
  level1_bmp = transpose(level1_bmp, [1, 2, 0])

  foreach dir, dirs, di do begin
    if (~file_test(dir, /directory)) then begin
      message, 'not directory: ' + dir
    endif

    dirname = file_basename(file_expand_path(dir))

    child = widget_info(self.tree, /child)
    if (child eq 0L) then begin
      self->set_title, dirname
    endif else begin
      self->set_title, 'several directories'
    endelse

    self->set_status, 'Loading ' + dirname + '...'

    ; add subdirs of dir as nodes, uname='datedir'
    datedirs = file_search(filepath(_filter, root=dir), $
                           /test_directory, $
                           count=n_datedirs)

    levels = lonarr(n_datedirs)
    n_files = lonarr(n_datedirs)

    for d = 0L, n_datedirs - 1L do begin
      if ((d + 1) mod 10 eq 0) then begin
        self->set_status, string(dirname, d + 1, n_datedirs, $
                                 format='(%"Loading %s: %d/%d directories...")')
      endif

      levels[d] = comp_dir_browser_findlevel(datedirs[d], files=files, $
                                             n_files=n_datedir_files)
      n_files[d] = n_datedir_files
    endfor

    self->set_status, string(dirname, $
                             format='(%"Finished loading %s, updating interface...")')

    widget_control, self.tlb, update=0

    case levels[0] of
      -1: bitmap = bytarr(16, 16, 3)
      0: bitmap = raw_bmp
      1: bitmap = level1_bmp
      2: bitmap = level1_bmp
    endcase

    ; add dir as root of tree
    if (n_elements(directory_id) gt 0L) then begin
      root = directory_id[di]

      ; eliminate existing directories
      n_old_datedirs = widget_info(directory_id[di], /n_children)
      all_datedirs = widget_info(directory_id[di], /all_children)
      for c = 0L, n_old_datedirs - 1L do begin
        widget_control, all_datedirs[c], /destroy
      endfor
    endif else begin
      root = widget_tree(self.tree, value=dirname, /folder, /expanded, $
                         uvalue={fullpath: file_expand_path(dir), filter: _filter}, $
                         bitmap=bitmap, $
                         uname='root', $
                         tooltip=file_expand_path(dir))
    endelse

    for d = 0L, n_datedirs - 1L do begin
      ; TODO: not using BITMAP keyword right now because it slows the creating
      ; of the tree nodes by a factor of 10 or more
      datedir = widget_tree(root, $
                            value=file_basename(datedirs[d]) $
                                    + ' - ' + strtrim(n_files[d], 2) + ' files', $
                            ;bitmap=bitmap, $
                            uvalue=datedirs[d], $
                            uname='datedir', $
                            tooltip=file_expand_path(datedirs[d]))
    endfor
    widget_control, self.tlb, update=1
  endforeach

  t1 = systime(/seconds)

  ;self->set_status, string(t1 - t0, format='(%"%0.1f seconds to load")')
  self->set_status, 'Ready'
end


;+
; Load a date directory.
;
; :Params:
;   datedir : in, required, type=string
;     directory with name of the form YYYYMMDD which contains CoMP
;     data files
;
; :Keywords:
;   reload : in, optional, type=boolean
;     set to reload a datedir and not use the cache
;-
pro comp_dir_browser::load_datedir, datedir, reload=reload
  compile_opt strictarr

  if (self.inventories->hasKey(datedir) && ~keyword_set(reload)) then begin
    files_info = (self.inventories)[datedir]

    *(self.files) = (self.files_cache)[datedir]
    n_files = n_elements(*(self.files))
    n_images = total(long(files_info.n_images), /integer)
  endif else begin
    loading_verb = keyword_set(reload) ? 'Reloading' : 'Loading'

    self->set_status, loading_verb + ' ' + datedir + '...'

    files = file_search(datedir, '*.fts*', count=n_files, /fold_case)

    *(self.files) = files
    (self.files_cache)[datedir] = n_files eq 0L ? [] : files

    if (n_files eq 0L) then begin
      widget_control, self.table, ysize=0
      file_info = {}
      n_images = 0L
    endif else begin
      datetime_key = strarr(n_files)
      type_key = lonarr(n_files)

      files_info = replicate(self->comp_dir_browser_row(), n_files)

      for f = 0L, n_files - 1L do begin
        if ((f + 1) mod 10 eq 0) then begin
          self->set_status, string(loading_verb, datedir, f + 1, n_files, $
                                   format='(%"%s %s: %d/%d files...")')
        endif

        ; set time fields
        basename = file_basename(files[f])
        file_tokens = strsplit(basename, '.', /extract, count=n_files_tokens)
        time = file_tokens[1]
        time = strmid(time, 0, 2) $
                 + ':' + strmid(time, 2, 2) $
                 + ':' + strmid(time, 4, 2)

        comp_query_file, files[f], $
                         beam_state=beam, wavelength=wave, $
                         polarization=pol, type=type, $
                         exposures=exposures, cover=cover, $
                         observation_id=obs_id, observation_plan=obs_plan, $
                         cal_polarizer=polarizer, cal_retarder=retarder, $
                         pol_angle=pol_angle

        n = n_elements(pol)
        case type of
          'OPAL': begin
              files_info[f].type = 'flat'
              files_info[f].n_images = strtrim(n, 2)
              if (basename eq 'flat.fts') then begin
                files_info[f].time = ''
                datetime_key[f] = ''
                type_key[f] = 1
              endif else begin
                files_info[f].time = time
                datetime_key[f] = strmid(basename, 0, 15)
              endelse
            end
          'CALIBRATION': begin
              files_info[f].type = 'calibration'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = time
              datetime_key[f] = strmid(basename, 0, 15)
            end
          'DATA': begin
              files_info[f].type = 'data'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = time
              datetime_key[f] = strmid(basename, 0, 15)
            end
          'BACKGROUND': begin
              files_info[f].type = 'background'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = time
              type_key[f] = 1
              datetime_key[f] = strmid(basename, 0, 15)
            end
          'DARK': begin
              files_info[f].type = 'dark'
              files_info[f].n_images = strtrim(n, 2)
              if (basename eq 'dark.fts') then begin
                files_info[f].time = ''
                datetime_key[f] = ''
                type_key[f] = 0
              endif else begin
                files_info[f].time = time
                datetime_key[f] = strmid(basename, 0, 15)
              endelse
            end
          'DYNAMICS': begin
              files_info[f].type = 'dynamics'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = time
              type_key[f] = 3
              datetime_key[f] = strmid(basename, 0, 15)
            end
          'POLARIZATION': begin
              files_info[f].type = 'polarization'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = time
              type_key[f] = 4
              datetime_key[f] = strmid(basename, 0, 15)
            end
          'INTENSITY': begin
              files_info[f].type = 'intensity'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = time
              type_key[f] = 2
              datetime_key[f] = strmid(basename, 0, 15)
            end
          'MEAN': begin
              files_info[f].type = 'mean'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = ''
              datetime_key[f] = ''
              type_key[f] = 2
            end
          'MEDIAN': begin
              files_info[f].type = 'median'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = ''
              datetime_key[f] = ''
              type_key[f] = 3
            end
          'QUICK_INVERT': begin
              files_info[f].type = 'quick_invert'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = ''
              datetime_key[f] = ''
              type_key[f] = 5
            end
          'SIGMA': begin
              files_info[f].type = 'sigma'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = ''
              datetime_key[f] = ''
              type_key[f] = 4
            end
          'INVALID': begin
              files_info[f].type = 'invalid'
              files_info[f].n_images = strtrim(n, 2)
              files_info[f].time = ''
              datetime_key[f] = ''
              type_key[f] = -1
            end
          else: begin
              files_info[f].type = 'unknown'
              self->set_status, string(type, file_basename(files[f]), $
                                       format='(%"unknown data file type %s for %s")')
            end
        endcase

        if (n gt 0L) then begin
          exposures_ind = where(finite(exposures), n_exposures)
          if (n_exposures gt 0L) then begin
            exposures = exposures[exposures_ind]
            exposures = exposures[uniq(exposures, sort(exposures))]
            n_exposures = n_elements(exposures)
          endif
          case n_exposures of
            0: files_info[f].exposure = ''
            1: files_info[f].exposure = string(exposures[0], format='(%"%0.1f ms")')
            else: files_info[f].exposure = strjoin(string(exposures, format='(F0.1)'), ', ') + ' ms'
          endcase

          uniq_pol = pol[uniq(pol, sort(pol))]
          pol_ind = where(uniq_pol ne '', n_valid_pol)
          if (n_valid_pol eq 0L) then begin
            files_info[f].pol_states = ''
          endif else begin
            files_info[f].pol_states = strjoin(strtrim(uniq_pol[pol_ind], 2), ', ')
          endelse

          wave_ind = where(finite(wave), n_wave)
          if (n_wave eq 0L) then begin
            files_info[f].wavelengths = ''
          endif else begin
            wave = wave[wave_ind]
            uniq_wave = wave[uniq(wave, sort(wave))]
            files_info[f].wavelengths = strtrim(n_elements(uniq_wave), 2) + ': ' + strjoin(strtrim(uniq_wave, 2), ', ') + ' nm'
          endelse

          files_info[f].obs_plan = obs_plan
          files_info[f].obs_id = obs_id
          if (self.calibration) then begin
            files_info[f].polarizer = polarizer ? 'IN' : ''
            files_info[f].retarder = retarder ? 'IN' : ''
            files_info[f].pol_angle = finite(pol_angle) $
                                      ? string(pol_angle, format='(%"%7.1f")') $
                                      : ''
          endif
        endif
      endfor
      ind = mg_sort(datetime_key, type_key)
      files_info = files_info[ind]
      *(self.files) = files[ind]
      (self.files_cache)[datedir] = files[ind]
      n_images = total(long(files_info.n_images), /integer)
    endelse

    (self.inventories)[datedir] = files_info
  endelse

  self->filter_table

  self->set_status, string(n_images, n_files, $
                           format='(%"Loaded %d images in %d files")')
end


;+
; Set the background color for the cells with rows corresponding to `name`.
;
; :Params:
;   files_info : in, required, type=array of structures
;     the files information structure
;   name : in, required, type=string
;     name of the type of rows
;   tcolors : in, out, required, type="bytarr(3, n_rows)"
;     background colors
;
; :Keywords:
;   n_cols : in, required, type=integer
;     number of columns
;   color : in, required, type=bytarr(3)
;     color to use for these rows
;-
pro comp_dir_browser::_table_colors, files_info, name, tcolors, $
                                     n_cols=n_cols, color=color
  compile_opt strictarr

  ind = where(files_info.type eq name, n_rows)
  if (n_rows gt 0) then begin
    _ind = rebin(reform(ind * n_cols, 1, n_rows), n_cols, n_rows) $
             + rebin(reform(lindgen(n_cols), n_cols, 1), n_cols, n_rows)
    tcolors[0, _ind] = color[0]
    tcolors[1, _ind] = color[1]
    tcolors[2, _ind] = color[2]
  endif
end


;+
; Filter the table according to the checkboxes about showing darks, flats, 1074,
; 1079, 1083, backgrounds, and level 2 files.
;-
pro comp_dir_browser::filter_table
  compile_opt strictarr

  widget_control, self.current_datedir, get_uvalue=datedir

  files_info = (self.inventories)[datedir]
  n_files = n_elements(files_info)

  if (n_files gt 0L) then begin
    keep_type = bytarr(n_files)
    if (self.show_darks) then keep_type or= files_info.type eq 'dark'
    if (self.show_flats) then keep_type or= files_info.type eq 'flat'
    if (self.show_data) then keep_type or= files_info.type eq 'data'
    if (self.show_backgrounds) then keep_type or= files_info.type eq 'background'
    if (self.show_level2) then begin
      keep_type or= files_info.type eq 'dynamics' $
                      or files_info.type eq 'polarization' $
                      or files_info.type eq 'mean' $
                      or files_info.type eq 'median' $
                      or files_info.type eq 'sigma' $
                      or files_info.type eq 'quick_invert'
    endif

    re = '.*: ([[:digit:].]+).*'
    w = stregex(files_info.wavelengths, re, /extract, /subexpr)
    w = reform(w[1, *])
    central_wavelengths = [1074.62, 1079.8, 1083.0]
    central_wavelengths = rebin(reform(central_wavelengths, $
                                       3, $
                                       1), $
                                3, $
                                n_elements(w))
    w = rebin(reform(float(w), $
                     1, $
                     n_elements(w)), $
              3, $
              n_elements(w))

    mins = min(abs(w - central_wavelengths), index, dimension=1)
    wave_type = index mod 3

    keep_wave = bytarr(n_files)
    if (self.show_1074) then keep_wave or= wave_type eq 0
    if (self.show_1079) then keep_wave or= wave_type eq 1
    if (self.show_1083) then keep_wave or= wave_type eq 2

    ; L1 flat and dark files are considered all wave types
    keep_wave or= file_basename(*(self.files)) eq 'flat.fts' $
                    or file_basename(*(self.files)) eq 'dark.fts'

    *self.current_filter = keep_type and keep_wave
    ind = where(*self.current_filter, n_files)
    if (n_files gt 0L) then begin
      files_info = files_info[ind]
    endif else begin
      files_info = []
    endelse
  endif

  widget_control, self.table, set_value=files_info, ysize=n_files
  if (n_files gt 0) then begin
    n_cols = n_tags(self->comp_dir_browser_row())
    bcolors = bytarr(3, n_files * n_cols)
    bcolors[0, *] = 200
    bcolors[1, *] = 200
    bcolors[2, *] = 200

    self->_table_colors, files_info, 'dark', bcolors, $
                         n_cols=n_cols, color=bytarr(3) + 140B
    self->_table_colors, files_info, 'flat', bcolors, $
                         n_cols=n_cols, color=bytarr(3) + 160B
    self->_table_colors, files_info, 'calibration', bcolors, $
                         n_cols=n_cols, color=[255B, 255B, 200B]
    self->_table_colors, files_info, 'background', bcolors, $
                         n_cols=n_cols, color=bytarr(3) + 180B
    self->_table_colors, files_info, 'dynamics', bcolors, $
                         n_cols=n_cols, color=[255B, 200B, 200B]
    self->_table_colors, files_info, 'polarization', bcolors, $
                         n_cols=n_cols, color=[255B, 220B, 220B]
    self->_table_colors, files_info, 'mean', bcolors, $
                         n_cols=n_cols, color=[230B, 255B, 230B]
    self->_table_colors, files_info, 'median', bcolors, $
                         n_cols=n_cols, color=[230B, 255B, 230B]
    self->_table_colors, files_info, 'quick_invert', bcolors, $
                         n_cols=n_cols, color=[230B, 255B, 230B]
    self->_table_colors, files_info, 'sigma', bcolors, $
                         n_cols=n_cols, color=[230B, 255B, 230B]
    self->_table_colors, files_info, 'invalid', bcolors, $
                         n_cols=n_cols, color=[255B, 0B, 0B]

    widget_control, self.table, background_color=bcolors
  endif
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

        toolbar = widget_info(self.tlb, find_by_uname='toolbar')

        tlb_geometry = widget_info(self.tlb, /geometry)
        content_base_geometry = widget_info(widget_info(self.tlb, /child), /geometry)
        tree_geometry = widget_info(self.tree, /geometry)
        table_geometry = widget_info(self.table, /geometry)
        statusbar_geometry = widget_info(self.statusbar, /geometry)
        toolbarbar_geometry = widget_info(toolbar, /geometry)

        table_width = event.x - tree_geometry.scr_xsize $
                        - 2 * tlb_geometry.xpad $
                        - content_base_geometry.xpad $
                        - 3
        statusbar_width = table_width + tree_geometry.scr_xsize $
                            + content_base_geometry.xpad
        height = event.y - 3 * tlb_geometry.ypad $
                   - 2 * content_base_geometry.ypad $
                   - statusbar_geometry.scr_ysize $
                   - 2 * statusbar_geometry.margin $
                   - toolbarbar_geometry.scr_ysize $
                   - 2 * toolbarbar_geometry.margin

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
                              set_table_select=[0, $
                                                event.sel_top, $
                                                n_elements(self->_colwidths()) - 1L, $
                                                event.sel_bottom]
              widget_control, self.table, set_table_view=current_view
              self.selection = [event.sel_top, event.sel_bottom]
              if (event.sel_top eq event.sel_bottom) then begin
                ind = where(*self.current_filter, n_files)
                filename = ((*(self.files))[ind])[event.sel_top]
                self->set_status, file_basename(filename)
              endif
            end
          'WIDGET_CONTEXT': begin
              widget_displaycontextmenu, event.id, event.x, event.y, self.context_base
            end
          else:
        endcase
      end
    'refresh': begin
        selected_id = widget_info(self.tree, /tree_select)
        treenode_uname = widget_info(selected_id, /uname)
        widget_control, selected_id, get_uvalue=treenode_uvalue
        case treenode_uname of
          'root': begin
              ; reload directory list (throw out caches)
              self->load_directory, treenode_uvalue.fullpath, $
                                    filter=treenode_uvalue.filter, $
                                    directory_id=selected_id
            end
          'datedir': begin
              ; reload datedir (don't use cache)
              self->load_datedir, treenode_uvalue, /reload
            end
          else: begin
              self->set_status, 'invalid tree node uname: ' + treenode_uname
            end
        endcase
      end
    'filter_darks': begin
        self.show_darks = event.select
        self->filter_table
      end
    'filter_flats': begin
        self.show_flats = event.select
        self->filter_table
      end
    'filter_1074': begin
        self.show_1074 = event.select
        self->filter_table
      end
    'filter_1079': begin
        self.show_1079 = event.select
        self->filter_table
      end
    'filter_1083': begin
        self.show_1083 = event.select
        self->filter_table
      end
    'filter_data': begin
        self.show_data = event.select
        self->filter_table
      end
    'filter_backgrounds': begin
        self.show_backgrounds = event.select
        self->filter_table
      end
    'filter_level2': begin
        self.show_level2 = event.select
        self->filter_table
      end
    'display_files': begin
        if (~obj_valid(self.file_browser)) then begin
          resolve_routine, 'comp_browser'
          self.file_browser = mg_fits_browser(classname='comp_browser', $
                                              group_leader=self.tlb, /floating)
        endif

        ind = where(*self.current_filter, n_files)
        filenames = ((*(self.files))[ind])[self.selection[0]:self.selection[1]]
        self.file_browser->load_files, filenames
      end
    'compute_totals': begin
        widget_control, self.table, get_value=files_info
        n_images = total(long(files_info.n_images), /integer)
        n_files = n_elements(files_info)
        format = '(%"Loaded %d images in %d files")'
        self->set_status, string(n_images, $
                                 n_files, $
                                 format=format)
      end
    'average_files': begin
        ind = where(*self.current_filter, n_files)
        filenames = ((*(self.files))[ind])[self.selection[0]:self.selection[1]]

        dir_node = widget_info(self.current_datedir, /parent)
        widget_control, dir_node, get_uvalue=uvalue

        full_filenames = filepath(filenames, root=uvalue.fullpath)
        state = comp_average_dialog(full_filenames, dialog_parent=self.tlb, $
                                    output_filename=output_filename, $
                                    method=method)
        if (state) then begin
          method_name = method ? 'Median' : 'Mean'
          self->set_status, string(method_name, n_elements(filenames), output_filename, $
                                   format='(%"%s from %d files is in %s")')
        endif
      end
    'root': begin
        widget_control, event.id, get_uvalue=s
        self->set_status, s.fullpath
      end
    'datedir': begin
        self.current_datedir = event.id
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
function comp_dir_browser::_colwidths
  compile_opt strictarr

  colwidths = self.calibration $
              ? [0.15, 0.1, 0.1, 0.12, 0.2775, 0.2775, 0.15, 0.15, 0.1, 0.1, 0.1] $
              : [0.15, 0.1, 0.1, 0.12, 0.2775, 0.2775, 0.15, 0.15]
  return, colwidths / total(colwidths) * 0.975
end


;+
; Create the widget hierarchy.
;-
pro comp_dir_browser::create_widgets
  compile_opt strictarr

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb')

  bitmapdir = ['resource', 'bitmaps']
  toolbar = widget_base(self.tlb, /toolbar, /row, uname='toolbar')
  refresh_data = read_png(filepath('refresh.png', root=mg_src_root()))
  refresh_data = transpose(refresh_data, [1, 2, 0])
  refresh_data = congrid(refresh_data, 16, 16, 4, /interp)
  refresh_button = widget_button(toolbar, /bitmap, uname='refresh', $
                                 tooltip='Refresh', $
                                 value=refresh_data)

  filter_type_base = widget_base(toolbar, /nonexclusive, /row, xpad=10, ypad=0, frame=1)
  darks_button = widget_button(filter_type_base, value='Darks', uname='filter_darks')
  flats_button = widget_button(filter_type_base, value='Flats', uname='filter_flats')
  data_button = widget_button(filter_type_base, value='Data', uname='filter_data')
  background_button = widget_button(filter_type_base, value='Backgrounds', uname='filter_backgrounds')
  l2_button = widget_button(filter_type_base, value='Level 2', uname='filter_level2')

  widget_control, filter_type_base, set_button=1

  filter_wave_base = widget_base(toolbar, /nonexclusive, /row, xpad=10, ypad=0, frame=1)
  data_1074_button = widget_button(filter_wave_base, value='1074', uname='filter_1074')
  data_1079_button = widget_button(filter_wave_base, value='1079', uname='filter_1079')
  data_1083_button = widget_button(filter_wave_base, value='1083', uname='filter_1083')

  widget_control, filter_wave_base, set_button=1

  ; content row
  content_base = widget_base(self.tlb, /row, xpad=0)

  tree_xsize = 250
  table_xsize = 850
  scr_ysize = 600
  xpad = 0

  self.tree = widget_tree(content_base, uname='browser', $
                          scr_xsize=tree_xsize, scr_ysize=scr_ysize)

  col_titles = ['Type', $
                'Time', $
                'Exposure', $
                'No. images', $
                'Wavelengths', $
                'Pol states', $
                'Obs plan', $
                'Obs ID']
  if (self.calibration) then begin
    col_titles = [col_titles, $
                  'Polarizer', $
                  'Retarder', $
                  'Pol angle']
  endif

  self.table = widget_table(content_base, $
                            /no_row_headers, $
                            column_labels=col_titles, $
                            column_widths=self->_colwidths() * table_xsize, $
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
  average_button = widget_button(self.context_base, value='Average files', $
                                 uname='average_files')

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

  ptr_free, self.files, self.current_filter
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
;   calibration : in, optional, type=boolean
;     set to display calibration related fields
;-
function comp_dir_browser::init, directory=directory, tlb=tlb, $
                                 calibration=calibration
  compile_opt strictarr

  self.title = 'CoMP data directory browser'

  self.calibration = keyword_set(calibration)

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self.inventories = hash()
  self.files_cache = hash()

  self->set_status, 'Ready'

  self.current_filter = ptr_new(/allocate_heap)
  self.show_darks = 1B
  self.show_flats = 1
  self.show_1074 = 1B
  self.show_1079 = 1B
  self.show_1083 = 1B
  self.show_data = 1B
  self.show_backgrounds = 1B
  self.show_level2 = 1B

  if (arg_present(tlb)) then tlb = self.tlb
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
             calibration: 0B, $
             date_dir: '', $
             tlb: 0L, $
             tree: 0L, $
             current_datedir: 0L, $
             current_filter: ptr_new(), $
             table: 0L, $
             show_darks: 0B, $
             show_flats: 0B, $
             show_1074: 0B, $
             show_1079: 0B, $
             show_1083: 0B, $
             show_data: 0B, $
             show_backgrounds: 0B, $
             show_level2: 0B, $
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
;   directory : in, optional, type=string/strarr
;     directory/directories to browse
;   calibration : in, optional, type=boolean
;     set to display calibration related fields; only valid on newly created
;     CoMP data browsers
;-
pro comp_dir_browser, pdirectory, directory=kdirectory, calibration=calibration, filter=filter
  compile_opt strictarr
  on_error, 2
  common comp_dir_browser, browser

  _dir = n_elements(pdirectory) gt 0L $
           ? pdirectory $
           : (n_elements(kdirectory) gt 0L ? kdirectory : '')
  if (n_elements(_dir) eq 1 && _dir eq '') then message, 'directory not specified'

  if (~obj_valid(browser)) then begin
    browser = obj_new('comp_dir_browser', calibration=calibration)
  endif
  browser->load_directory, _dir, filter=filter
end


; main-level example program

comp_dir_browser, '/hao/mlsodata1/Data/CoMP/raw'
comp_dir_browser, '/hao/kaula1/Data/CoMP/process'

end
