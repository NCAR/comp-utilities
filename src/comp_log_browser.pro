; docformat = 'rst'


;= helper routines

;+
; Thin procedural wrapper to call `::handle_events` event handler.
;
; :Params:
;    event : in, required, type=structure
;       event structure for event handler to handle
;-
pro comp_log_browser_handleevents, event
  compile_opt strictarr

  widget_control, event.top, get_uvalue=browser
  browser->handle_events, event
end


;+
; Thin procedural wrapper to call `::cleanup_widgets` cleanup routine.
;
; :Params:
;    tlb : in, required, type=long
;       top-level base widget identifier
;-
pro comp_log_browser_cleanup, tlb
  compile_opt strictarr

  widget_control, tlb, get_uvalue=browser
  if (obj_valid(browser)) then browser->cleanup_widgets
end


;+
; Filter the cidx log by the current log level and display the correct text.
;-
pro comp_log_browser::_filter
  compile_opt strictarr

  date = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}'
  time = '[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}'
  level = '([[:alpha:]]*):'
  re = string(date, time, level, format='(%"^%s %s %s")')

  tokens = stregex(*self.cidx_logtext, re, /extract, /subexpr)
  levels = tokens[1, *]

  mask = bytarr(n_elements(levels))
  switch self.log_level of
    5: mask or= levels eq 'DEBUG'
    4: mask or= levels eq 'INFO'
    3: mask or= levels eq 'WARN'
    2: mask or= levels eq 'ERROR'
    1: mask or= levels eq 'CRITICAL'
  endswitch

  ind = where(mask, count)
  if (count gt 0L) then begin
    filtered_text = (*self.cidx_logtext)[ind]
  endif else filtered_text = ''
  widget_control, self.cidx_text, set_value=filtered_text
end


;+
; Load the contents of a file into the cidx log, but do not display. Call
; `_filter` to filter text by the current log level and display.
;
; :Params:
;   filename : in, required, type=string
;     filename of text file to load as cidx log
;-
pro comp_log_browser::_load_text_file, filename
  compile_opt strictarr

  if (~file_test(filename)) then return

  n_lines = file_lines(filename)
  text = strarr(n_lines)
  openr, lun, filename, /get_lun
  readf, lun, text
  free_lun, lun

  *self.cidx_logtext = text
end


;+
; Set the window title based on the current filename. Set the filename to the
; empty string if there is no title to display.
;
; :Params:
;    filename : in, required, type=string
;       filename to display in title
;-
pro comp_log_browser::set_title, filename
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
pro comp_log_browser::set_status, msg, clear=clear
  compile_opt strictarr

  _msg = keyword_set(clear) || n_elements(msg) eq 0L ? '' : msg
  widget_control, self.statusbar, set_value=_msg
end


pro comp_log_browser::set_level, level
  compile_opt strictarr

  self.log_level = level
  unames = 'filter_' + ['critical', 'error', 'warning', 'info', 'debug']
  button = widget_info(self.tlb, find_by_uname=unames[level - 1L])
  widget_control, button, /set_button
end


;+
; Add a directory to the directory browser.
;
; :Params:
;   dir : in, required, type=string
;     directory name to load
;-
pro comp_log_browser::load_directory, dir
  compile_opt strictarr

  if (file_test(dir, /directory)) then begin
    ; find available dates
    cidx_logs = file_search(filepath('*.log', subdir='cidx', root=dir), count=n_logs)
    dates = strmid(file_basename(cidx_logs), 0, 8)
    widget_control, self.list, set_value=dates

    ; store state
    *self.dates = dates
    self.log_dir = dir

    self->set_title, file_basename(dir)
  endif
end


pro comp_log_browser::load_observer_directory, dir
  compile_opt strictarr

  if (file_test(dir, /directory)) then begin
    self.obs_log_dir = dir
  endif
end



;= event handling

;+
; Handle all events from the widget program.
;
; :Params:
;    event : in, required, type=structure
;       event structure for event handler to handle
;-
pro comp_log_browser::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'tlb': begin
        ; TODO: implement resizing
    end
    'list': begin
        date = (*self.dates)[event.index]
        cidx_log_filename = filepath(date + '.log', subdir='cidx', root=self.log_dir)

        self->_load_text_file, cidx_log_filename

        ; load observer log, if possible
        if (self.obs_log_dir ne '') then begin
          year = strmid(date, 0, 4)
          month = strmid(date, 4, 2)
          day = strmid(date, 6, 2)
          doy = mg_ymd2doy(long(year), long(month), long(day))

          obs_basename = string(year, doy, format='(%"mlso.%sd%03d.olog")')
          obs_log_filename = filepath(obs_basename, subdir=year, root=self.obs_log_dir)

          self->_load_text_file, obs_log_filename, self.obs_text
        endif else self->set_status, 'No observer logs found'
      end
    'filter_debug': self.log_level = 5
    'filter_info': self.log_level = 4
    'filter_warning': self.log_level = 3
    'filter_error': self.log_level = 2
    'filter_critical': self.log_level = 1
    else:
  endcase

  self->_filter
end


;= widget lifecycle methods

;+
; Handle cleanup when the widget program is destroyed.
;-
pro comp_log_browser::cleanup_widgets
  compile_opt strictarr

  obj_destroy, self
end


;+
; Create the widget hierarchy.
;-
pro comp_log_browser::create_widgets
  compile_opt strictarr

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb')
  
  ; toolbar
  bitmapdir = ['resource', 'bitmaps']
  toolbar = widget_base(self.tlb, /toolbar, /row, uname='toolbar', xpad=0)

  file_toolbar = widget_base(toolbar, /toolbar, /row, xpad=0)
  open_button = widget_button(file_toolbar, /bitmap, uname='open', $
                              tooltip='Open FITS file', $
                              value=filepath('open.bmp', subdir=bitmapdir))

  log_level_toolbar = widget_base(toolbar, /toolbar, /row)
  log_levels_base = widget_base(log_level_toolbar, /row, xpad=0, ypad=0, /exclusive)
  critical_button = widget_button(log_levels_base, value='Critical', $
                                  accelerator='Shift+1', uname='filter_critical')
  error_button = widget_button(log_levels_base, value='Error', $
                               accelerator='Shift+2', uname='filter_error')
  warning_button = widget_button(log_levels_base, value='Warning', $
                                 accelerator='Shift+3', uname='filter_warning')
  info_button = widget_button(log_levels_base, value='Info', $
                              accelerator='Shift+4', uname='filter_info')
  debug_button = widget_button(log_levels_base, value='Debug', $
                               accelerator='Shift+5', uname='filter_debug')

  ; content row
  content_base = widget_base(self.tlb, /row, xpad=0)

  list_xsize = 125
  text_xsize = 850
  scr_ysize = 600
  xpad = 0

  self.list = widget_list(content_base, uname='list', $
                          scr_xsize=list_xsize, scr_ysize=scr_ysize)

  tabs = widget_tab(content_base, uname='tabs')

  cidx_base = widget_base(tabs, xpad=0, ypad=0, title='Run log', /column)
  self.cidx_text = widget_text(cidx_base, value='', uname='cidx', $
                               scr_xsize=text_xsize, scr_ysize=scr_ysize, $
                               /scroll)

  obs_base = widget_base(tabs, xpad=0, ypad=0, title='Observer log', /column)
  self.obs_text = widget_text(obs_base, value='', uname='obs', $
                              scr_xsize=text_xsize, scr_ysize=scr_ysize, $
                              /scroll)

  ; status bar
  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=list_xsize + text_xsize + 2 * 4.0, $
                                /align_left, /sunken_frame)
end


;+
; Draw the widget hierarchy.
;-
pro comp_log_browser::realize_widgets
  compile_opt strictarr

  widget_control, self.tlb, /realize
end


;+
; Start `XMANAGER`.
;-
pro comp_log_browser::start_xmanager
  compile_opt strictarr

  xmanager, 'comp_log_browser', self.tlb, /no_block, $
            event_handler='comp_log_browser_handleevents', $
            cleanup='comp_log_browser_cleanup'
end


pro comp_log_browser::cleanup
  compile_opt strictarr

  ptr_free, self.dates, self.cidx_logtext
end


function comp_log_browser::init
  compile_opt strictarr

  self.title = 'CoMP log browser'

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self.dates = ptr_new(/allocate_heap)
  self.cidx_logtext = ptr_new(/allocate_heap)
  self->set_level, 5L

  self->set_status, 'Ready'

  return, 1
end


pro comp_log_browser__define
  compile_opt strictarr

  define = { comp_log_browser, $
             tlb: 0L, $
             statusbar: 0L, $
             list: 0L, $
             cidx_text: 0L, $
             obs_text: 0L, $
             dates: ptr_new(), $
             log_dir: '', $
             obs_log_dir: '', $
             title: '', $
             log_level: 0, $
             cidx_logtext: ptr_new() $
           }
end


pro comp_log_browser, log_dir, observer_log_dir=observer_log_dir
  compile_opt strictarr
  on_error, 2
  common comp_log_browser, browser

  if (n_elements(log_dir) eq 0L) then begin
    message, 'log directory not specified'
  endif

  if (~obj_valid(browser)) then begin
    browser = obj_new('comp_log_browser')
  endif

  browser->load_directory, log_dir

  if (n_elements(observer_log_dir)) then begin
    browser->load_observer_directory, observer_log_dir
  endif
end
