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
    else:
  endcase
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

  tree_xsize = 300
  scr_ysize = 512

  ; tree
  self.tree = widget_tree(content_base, uname='browser', $
                          scr_xsize=tree_xsize, scr_ysize=scr_ysize)

  ; status bar
  self.statusbar = widget_label(self.tlb, scr_xsize=tree_xsize + scr_ysize, $
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

  return, 1
end


pro comp_dir_browser__define
  compile_opt strictarr

  define = { comp_dir_browser, $
             directory: '', $
             tlb: 0L, $
             tree: 0L, $
             statusbar: 0L, $
             title: '', $
             file_browser: obj_new() $
           }
end


pro comp_dir_browser, pdirectory, directory=kdirectory
  compile_opt strictarr

  b = obj_new('comp_dir_browser')
end
