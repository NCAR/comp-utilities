; docformat = 'rst'

;+
; Dialog to show options for computing an average of the files passed to it.
;-

;+
; Event handler.
;
; :Params:
;   event : in, required, type=structure
;     event structure
;-
pro comp_average_dialog_handleevents, event
  compile_opt strictarr

  widget_control, event.top, get_uvalue=pstate

  uname = widget_info(event.id, /uname)
  case uname of
    'file_select': begin
        file_text = widget_info(event.top, find_by_uname='output_filename')
        widget_control, file_text, get_value=original_filename
        cd, current=cwd
        output_filename = dialog_pickfile(/write, file=original_filename, $
                                          dialog_parent=event.top, $
                                          default_extension='.fts', path=cwd)

        if (output_filename ne '') then begin
          output_filename = expand_path(output_filename)
          widget_control, file_text, set_value=output_filename
          (*pstate).output_filename = output_filename
        endif
      end
    'method': (*pstate).method = event.index
    'run' : begin
        (*pstate).run = 1B

        ; produce result
        comp_compute_average, (*pstate).filenames, (*pstate).method, $
                              output_filename=(*pstate).output_filename

        ; display result
        comp_browser, (*pstate).output_filename

        widget_control, event.top, /destroy
      end
    'cancel' : widget_control, event.top, /destroy
  endcase
end


;+
; Launch dialog which allows user to select options for calculating average
; result file.
;
; :Returns:
;   1 if result produced, 0 if cancelled
;
; :Params:
;   filenames : in, required, type=string/strarr
;     filenames to average together
;
; :Keywords:
;   dialog_parent : in, optional, type=long
;     widget identifier of parent widget
;   output_filename : out, optional, type=string
;     filename of output average file, empty string if cancelled
;   method : out, optional, type=long
;     index of averaging method: 0 for mean, 1 for median
;-
function comp_average_dialog, filenames, dialog_parent=dialog_parent, $
                              output_filename=output_filename, method=method
  compile_opt strictarr

  title = string(n_elements(filenames), $
                 n_elements(filenames) gt 1L ? 's' : '', $
                 format='(%"Average %d file%s")')

  pstate = ptr_new({filenames: filenames, $
                    output_filename: '', $
                    method: 0L, $
                    run: 0B})
  tlb = widget_base(title=title, /column, /modal, group_leader=dialog_parent, $
                    uvalue=pstate)

  file_row = widget_base(tlb, /row)
  file_label = widget_label(file_row, value='Output filename:')
  file_text = widget_text(file_row, uname='output_filename', scr_xsize=300, ysize=1)
  file_button = widget_button(file_row, value='Select', uname='file_select', $
                              scr_xsize=100)

  method_row = widget_base(tlb, /row)
  method_label = widget_label(method_row, value='Average method:')
  method_combobox = widget_combobox(method_row, value=['Mean', 'Median'], $
                                    uname='method')

  run_row = widget_base(tlb, /row, /base_align_center)
  run_button = widget_button(run_row, value='Run', uname='run', scr_xsize=100)
  cancel_button = widget_button(run_row, value='Cancel', uname='cancel', scr_xsize=100)

  widget_control, tlb, /realize

  xmanager, 'comp_average_dialog', tlb, $
            event_handler='comp_average_dialog_handleevents'

  output_filename = (*pstate).output_filename
  run = (*pstate).run
  method = (*pstate).method

  ptr_free, pstate

  return, run
end
