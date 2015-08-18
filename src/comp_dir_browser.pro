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
           n_dark: 0, $
           n_flat: 0, $
           n_data: 0, $
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

  self.directory = dir

  ; clear tree widget
  children = widget_info(self.tree, /all_children)
  for i = 0L, n_elements(children) - 1L do begin
    if (children[i] gt 0) then widget_control, children[i], /destroy
  endfor

  ; clear table widget
  widget_control, self.table, set_value=comp_dir_browser_row()

  ; add dir as root of tree
  ; add subdirs of dir as nodes, uname='date_dir'
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
    'date_dir': begin
        ; TODO: load table
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
             directory: '', $
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

  _dir = n_elements(pdirectory) gt 0L ? pdirectory : kdirectory
  b = obj_new('comp_dir_browser', directory=_dir)
end


; main-level example program

comp_dir_browser, '/hao/mlsodata1/Data/CoMP/raw'

end
