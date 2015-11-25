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


pro comp_db_plot::redraw
  compile_opt strictarr

  x = (*self.data).(self.current_xaxis)
  y = (*self.data).(self.current_yaxis)
  old_window = !d.window
  wset, self.draw_id
  plot, x, y
  wset, old_window
end


;= handle events

pro comp_db_plot::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'tlb': begin
        self->redraw
      end
    'xaxis': begin
        self.current_xaxis = event.index
        self->redraw
      end
    'yaxis': begin
        self.current_yaxis = event.index
        self->redraw
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

  draw_xsize = 600.0
  draw_ysize = 300.0
  xpad = 2.0

  bitmapdir = ['resource', 'bitmaps']

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb', xpad=xpad)

  toolbar = widget_base(self.tlb, /row, uname='toolbar', $
                        /base_align_center, space=0.0)
  xaxis_label = widget_label(toolbar, value='X-axis:')
  xaxis_list = widget_combobox(toolbar, $
                               value=(*self.fields).name, $
                               uname='xaxis')
  yaxis_label = widget_label(toolbar, value='Y-axis:')
  yaxis_list = widget_combobox(toolbar, $
                               value=(*self.fields).name, $
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

  ptr_free, self.fields, self.data
end


function comp_db_plot::init, fields=fields, data=data
  compile_opt strictarr

  self.title = 'Database query creator'

  self.fields = ptr_new(fields)
  self.data = ptr_new(data)

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self->redraw

  self->set_status, 'Ready.'

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
             fields: ptr_new(), $
             data: ptr_new() $
           }
end


pro comp_db_plot, fields=fields, data=data
  compile_opt strictarr

  plot_browser = obj_new('comp_db_plot', fields=fields, data=data)
end

