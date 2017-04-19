; docformat = 'rst'

;+
; Create a plot.
;-


;= helper routines

;+
; Thin procedural wrapper to call `::handle_events` event handler.
;
; :Params:
;   event : in, required, type=structure
;     event structure for event handler to handle
;-
pro comp_db_plot_handleevents, event
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
pro comp_db_plot_cleanup, tlb
  compile_opt strictarr

  widget_control, tlb, get_uvalue=browser
  browser->cleanup_widgets
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
pro comp_db_plot::set_title, filename
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
pro comp_db_plot::set_status, msg, clear=clear
  compile_opt strictarr

  _msg = keyword_set(clear) || n_elements(msg) eq 0L ? '' : msg
  widget_control, self.statusbar, set_value=_msg
end


function comp_db_plot::_date2jd, d
  compile_opt strictarr

  year   = long(strmid(d, 0, 4))
  month  = long(strmid(d, 5, 2))
  day    = long(strmid(d, 8, 2))
  hour   = long(strmid(d, 11, 2))
  minute = long(strmid(d, 14, 2))
  second = long(strmid(d, 17, 2))

  bad_years_ind = where(year eq 0, n_bad_years)
  if (n_bad_years gt 0L) then begin
    year[bad_years_ind] = 1
  endif

  jd = julday(month, day, year, hour, minute, second)

  if (n_bad_years gt 0L) then begin
    jd[bad_years_ind] = !values.f_nan
  endif

  return, jd
end


pro comp_db_plot::_axis, t, info, data=data, tickformat=tickformat, tickunits=tickunits
  compile_opt strictarr

  if (info.type eq 12) then begin
    data = self->_date2jd(t)

    ; use appropriate date/time format for time interval
    finite_ind = where(finite(data), count)
    if (count lt 2L) then begin
      date_label = label_date(date_format=['%Y-%N-%D %H:%I'])
    endif else begin
      diff = data[finite_ind[-1]] - data[finite_ind[0]]

      if (diff lt 1.0) then begin
        date_label = label_date(date_format=['%Y-%N-%D %H:%I']) 
      endif else begin
        date_label = label_date(date_format=['%Y-%N-%D']) 
      endelse
    endelse
    tickformat = 'LABEL_DATE'
    tickunits = 'Time'
  endif else begin
    data = t
    tickunits = 'Numeric'
  endelse
end


pro comp_db_plot::_draw, x, y, xinfo, yinfo, clear=clear, filename=filename
  compile_opt strictarr

  if (n_elements(filename) gt 0L) then begin
    original_device = !d.name
    set_plot, 'ps'
    device, filename=filename
    charsize = 1.0
  endif else begin
    old_window = !d.window
    wset, self.draw_id
    ; scale character size with display size
    charsize = (!d.x_size / 600.0) < (!d.y_size / 300.0) < 1.5
    charsize <= 1.25
  endelse

  device, get_decomposed=odec
  device, decomposed=1

  if (keyword_set(clear)) then begin
    erase, '000000'x
  endif else begin
    self->_axis, x, xinfo, data=_x, tickformat=xtickformat, tickunits=xtickunits
    self->_axis, y, yinfo, data=_y, tickformat=ytickformat, tickunits=ytickunits
    plot, _x, _y, $
          xstyle=9, ystyle=8, $
          xtitle=xinfo.name, $
          ytitle=yinfo.name, $
          xtickformat=xtickformat, $
          xtickunits=xtickunits, $
          ytickformat=ytickformat, $
          ytickunits=ytickunits, $
          psym=3, $
          charsize=charsize
    self->set_status, string(xinfo.name, yinfo.name, $
                             format='(%"Plotted %s vs %s")')
  endelse

  device, decomposed=odec
  if (n_elements(filename) gt 0L) then begin
    device, /close_file
    set_plot, original_device
  endif else begin
    wset, old_window
  endelse
end


pro comp_db_plot::redraw, filename=filename
  compile_opt strictarr

  x = (*self.data).(self.current_xaxis)
  y = (*self.data).(self.current_yaxis)

  xinfo = (*self.fields)[self.current_xaxis]
  yinfo = (*self.fields)[self.current_yaxis]

  if (xinfo.type eq 253 || xinfo.type eq 254) then begin
    self->set_status, 'ERROR: cannot plot string x-values'
    self->_draw, /clear
    return
  endif

  if (yinfo.type eq 253 || yinfo.type eq 254) then begin
    self->set_status, 'ERROR: cannot plot string y-values'
    self->_draw, /clear
    return
  endif

  self->_draw, x, y, xinfo, yinfo, filename=filename
end


;= handle events

pro comp_db_plot::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'tlb': begin
        tlb_geometry = widget_info(self.tlb, /geometry)
        toolbar = widget_info(self.tlb, find_by_uname='toolbar')
        toolbar_geometry = widget_info(toolbar, /geometry)
        statusbar_geometry = widget_info(self.statusbar, /geometry)

        draw_xsize = event.x - 2 * tlb_geometry.xpad
        draw_ysize = event.y - 2 * tlb_geometry.ypad - 2 * tlb_geometry.space $
                       - toolbar_geometry.scr_ysize $
                       - statusbar_geometry.scr_ysize
        widget_control, self.tlb, update=0
        widget_control, self.draw, scr_xsize=draw_xsize, $
                                   scr_ysize=draw_ysize 
        widget_control, self.statusbar, scr_xsize=draw_xsize
        widget_control, self.tlb, update=1

        self->redraw
      end
    'xaxis': begin
        self.current_xaxis = (*self.available_columns)[event.index]
        self->redraw
      end
    'yaxis': begin
        self.current_yaxis = (*self.available_columns)[event.index]
        self->redraw
      end
    'save': begin
        filename = dialog_pickfile(/write, dialog_parent=self.tlb)
        if (filename ne '') then begin
          self->redraw, filename=filename
          self->set_status, 'Plot created in ' + filename
        endif
      end
    else:
  endcase
end


;= widget lifecycle methods

;+
; Handle cleanup when the widget program is destroyed.
;-
pro comp_db_plot::cleanup_widgets
  compile_opt strictarr

  obj_destroy, self
end


pro comp_db_plot::create_widgets
  compile_opt strictarr

  draw_xsize = 700.0
  draw_ysize = 400.0
  xpad = 1.0

  bitmapdir = ['resource', 'bitmaps']

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb', xpad=xpad)

  toolbar = widget_base(self.tlb, /row, uname='toolbar', $
                        /base_align_center, space=10.0)

  save_button = widget_button(toolbar, /bitmap, uname='save', $
                              tooltip='Save', $
                              value=filepath('save.bmp', subdir=bitmapdir))

  xaxis_base = widget_base(toolbar, xpad=0.0, ypad=0.0, space=0.0, /row)
  xaxis_label = widget_label(xaxis_base, value='X-axis:')

  xaxis_list = widget_combobox(xaxis_base, $
                               value=((*self.fields).name)[*self.available_columns], $
                               uname='xaxis')
  yaxis_base = widget_base(toolbar, xpad=0.0, ypad=0.0, space=0.0, /row)
  yaxis_label = widget_label(yaxis_base, value='Y-axis:')
  yaxis_list = widget_combobox(yaxis_base, $
                               value=((*self.fields).name)[*self.available_columns], $
                               uname='yaxis')

  self.draw = widget_draw(self.tlb, xsize=draw_xsize, ysize=draw_ysize)

  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=draw_xsize + 2 * xpad, $
                                /align_left, /sunken_frame)
end


;+
; Draw the widget hierarchy.
;-
pro comp_db_plot::realize_widgets
  compile_opt strictarr

  widget_control, self.tlb, /realize
  widget_control, self.draw, get_value=draw_id
  self.draw_id = draw_id
end


;+
; Start `XMANAGER`.
;-
pro comp_db_plot::start_xmanager
  compile_opt strictarr

  xmanager, 'comp_db_plot', self.tlb, /no_block, $
            event_handler='comp_db_plot_handleevents', $
            cleanup='comp_db_plot_cleanup'
end


;= lifecycle methods

pro comp_db_plot::cleanup
  compile_opt strictarr

  ptr_free, self.fields, self.data, self.available_columns
end


function comp_db_plot::init, table, fields=fields, data=data
  compile_opt strictarr

  self.title = 'Plots of database table ' + table

  self.available_columns = ptr_new(where(fields.type ne 253 and fields.type ne 254))

  self.fields = ptr_new(fields)
  self.data = ptr_new(data)

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self->redraw

  return, 1
end


pro comp_db_plot__define
  compile_opt strictarr

  define = { comp_db_plot, $
             title: '', $
             tlb: 0L, $
             draw: 0L, $
             draw_id: 0L, $
             statusbar: 0L, $
             current_xaxis: 0L, $
             current_yaxis: 0L, $
             available_columns: ptr_new(), $
             fields: ptr_new(), $
             data: ptr_new() $
           }
end


pro comp_db_plot, table, fields=fields, data=data
  compile_opt strictarr

  plot_browser = obj_new('comp_db_plot', table, fields=fields, data=data)
end

