; docformat = 'rst'

;+
; Create a query.
;-


;= helper routines

;+
; Thin procedural wrapper to call `::handle_events` event handler.
;
; :Params:
;   event : in, required, type=structure
;     event structure for event handler to handle
;-
pro comp_db_query_handleevents, event
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
pro comp_db_query_cleanup, tlb
  compile_opt strictarr

  widget_control, tlb, get_uvalue=browser
  browser->cleanup_widgets
end


;= helper methods

pro comp_db_query::_set_tree_buttons
  compile_opt strictarr

  add_button = widget_info(self.tlb, find_by_uname='add')
  remove_button = widget_info(self.tlb, find_by_uname='remove')
  child_button = widget_info(self.tlb, find_by_uname='child')

  if (self.current_tree_node eq 0L) then begin
    widget_control, add_button, sensitive=0
    widget_control, remove_button, sensitive=0
    widget_control, child_button, sensitive=0
  endif else begin
    if (widget_info(self.current_tree_node, /type) eq 11) then begin
      widget_control, self.current_tree_node, get_uvalue=uvalue
      if (widget_info(self.current_tree_node, /uname) eq 'root') then begin
        widget_control, add_button, sensitive=0
        widget_control, remove_button, sensitive=0
        widget_control, child_button, sensitive=uvalue.type ne 2
      endif else begin
        widget_control, add_button, sensitive=1
        widget_control, remove_button, sensitive=1
        widget_control, child_button, sensitive=uvalue.type ne 2
      endelse
    endif
  endelse
end


function comp_db_query::_ops, index
  compile_opt strictarr

  ops = ['<', '<=', '>', '>=', '=']
  if (n_elements(index) gt 0L) then return, ops[index]
  return, ops
end


function comp_db_query::_get_op_index, op
  compile_opt strictarr

  ind = where(self->_ops() eq op, count)
  return, ind[0]
end


function comp_db_query::_get_field_index, field
  compile_opt strictarr

  ind = where(*self.fields eq field, count)
  return, ind[0]
end



pro comp_db_query::_set_condition_title
  compile_opt strictarr

  field_combobox = widget_info(self.tlb, find_by_uname='field')
  op_combobox = widget_info(self.tlb, find_by_uname='op')
  value_text = widget_info(self.tlb, find_by_uname='value')

  field = widget_info(field_combobox, /combobox_gettext)
  op = widget_info(op_combobox, /combobox_gettext)
  widget_control, value_text, get_value=value

  title = string(field, op, value, format='(%"%s %s ''%s''")')
  if (widget_info(self.current_tree_node, /valid)) then begin
    widget_control, self.current_tree_node, $
                    set_value=title, $
                    set_uvalue={type:2L, field:field, op:op, value:value}
  endif
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
pro comp_db_query::set_title, filename
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
pro comp_db_query::set_status, msg, clear=clear
  compile_opt strictarr

  _msg = keyword_set(clear) || n_elements(msg) eq 0L ? '' : msg
  widget_control, self.statusbar, set_value=_msg
end


function comp_db_query::export, root
  compile_opt strictarr

  _root = n_elements(root) eq 0L ? widget_info(self.tlb, find_by_uname='root') : root
  widget_control, _root, get_uvalue=uvalue

  case uvalue.type of
    0: begin
        children = widget_info(_root, /all_children)
        n_children = widget_info(_root, /n_children)
        if (n_children lt 1L) then return, ''
        queries = strarr(n_children)
        for c = 0L, n_children - 1L do begin
          queries[c] = self->export(children[c])
        endfor
        return, strjoin('(' + queries + ')', ' and ')
      end
    1: begin
        children = widget_info(_root, /all_children)
        n_children = widget_info(_root, /n_children)
        if (n_children lt 1L) then return, ''
        queries = strarr(n_children)
        for c = 0L, n_children - 1L do begin
          queries[c] = self->export(children[c])
        endfor
        return, strjoin('(' + queries + ')', ' or ')
      end
    2: begin
        return, string(uvalue.field, uvalue.op, uvalue.value, $
                       format='(%"%s %s ''%s''")')
      end
  endcase
end


function comp_db_query::_import_parse, query, start_index, length=length
  compile_opt strictarr

  if (start_index ge strlen(query)) then begin
    length = 0
    return, !null
  endif

  char = strmid(query, start_index, 1)
  bchar = (byte(char))[0]

  ; whitespace

  if (char eq ' ') then begin
    result = self->_import_parse(query, start_index + 1, length=slength)
    length = slength + 1
    return, result
  endif

  ; operator/symbol

  ops = [self->_ops(), '(', ')']
  ind = where(char eq ops, count)
  if (count gt 0L) then begin
    char2 = char + strmid(query, start_index + 1, 1)
    ind = where(char2 eq ops, count)
    if (count gt 0L) then begin
      length = 2
      return, char2
    endif else begin
      length = 1
      return, char
    endelse
  endif

  ; name

  ; ASCII 48 = '0', ASCII 57 = '9', 
  ; ASCII 65 = 'a', ASCII 90 = 'z', ASCII 97 = 'A', ASCII 122 = 'Z'
  if (char eq '_' $
        || (bchar ge 48 && bchar le 57) $
        || (bchar ge 65 && bchar le 90) $
        || (bchar ge 97 && bchar le 122)) then begin
    i = start_index + 1
    done = 0
    while (i lt strlen(query) && ~done) do begin
      char = strmid(query, i, 1)
      bchar = (byte(char))[0]
      done = (char eq '_' $
                || (bchar ge 48 && bchar le 57) $
                || (bchar ge 65 && bchar le 90) $
                || (bchar ge 97 && bchar le 122)) eq 0
      if (~done) then i++
    endwhile
    length = i - start_index
    return, strmid(query, start_index, length)
  endif

  ; value

  if (char eq '''') then begin
    i = start_index + 1
    done = 0
    while (i lt strlen(query) && ~done) do begin
      char = strmid(query, i, 1)
      bchar = (byte(char))[0]
      done = char eq ''''
      if (~done) then i++
    endwhile
    length = i - start_index + 1
    return, strmid(query, start_index, length)
  endif
end


function comp_db_query::_import_factor, stack, pos, tree=tree, level=level
  compile_opt strictarr

  print, level, pos, format='(%"%sfactor pos: %d")'

  if (stack[pos] eq '(') then begin
    pos++
    results = self->_import_expr(stack, pos, tree=tree, level=level + '  ')
    if (stack[pos] eq ')') then pos++ else message, 'expecting close parenthesis'
    return, results
  endif else begin
    return, stack[pos++]
  endelse
end


function comp_db_query::_import_term, stack, pos, tree=tree, level=level
  compile_opt strictarr

  print, level, pos, format='(%"%sterm pos: %d")'
  factor = self->_import_factor(stack, pos, tree=tree, level=level + '  ')

  ops = self->_ops()
  if (pos lt stack->count()) then ind = where(stack[pos] eq ops, count)

  while (pos lt stack->count() && count gt 0L) do begin
    op = ops[ind[0]]
    pos++
    print, level, pos, format='(%"%sterm pos: %d")'
    value = self->_import_factor(stack, pos, tree=tree, level=level + '  ')
    term = {type:2, field:factor, op:op, value:value}
    help, term
    return, term
    if (pos lt stack->count()) then ind = where(stack[pos] eq ops, count)
  endwhile

  print, level, format='(%"%sreturning factor")'
  help, factor
  return, factor
end


function comp_db_query::_import_expr, stack, pos, tree=tree, level=level
  compile_opt strictarr

  print, level, pos, format='(%"%sexpr pos: %d")'
  tree_uname = widget_info(tree, /uname)
  uvalue = self->_import_term(stack, pos, tree=tree, level=level + '  ')
  children = list()
  if (n_elements(uvalue) gt 0L) then children->add, uvalue
  while (pos lt stack->count() && (stack[pos] eq 'and' || stack[pos] eq 'or')) do begin
    conditional = strupcase(stack[pos])
    print, level, pos, conditional, format='(%"%sexpr pos: %d, conditional: %s")'
    type = conditional eq 'AND' ? 0L : 1L

    pos++
    print, level, pos, format='(%"%sexpr pos: %d")'
    uvalue = self->_import_term(stack, pos, tree=tree, level=level + '  ')
    if (n_elements(uvalue) gt 0L) then children->add, uvalue
  endwhile

  if (n_elements(conditional) gt 0L && children->count() gt 0L) then begin
    results = {type:type, children:children->toArray()}
  endif else results = !null

  obj_destroy, children

  return, results
end


pro comp_db_query::import, query
  compile_opt strictarr

  tree = widget_info(self.tlb, find_by_uname='tree')
  root = widget_info(self.tlb, find_by_uname='root')

  widget_control, root, /destroy

  stack = list()
  start_index = 0
  token = self->_import_parse(query, start_index, length=length)
  while (n_elements(token) gt 0) do begin
    stack->add, token
    start_index += length
    token = self->_import_parse(query, start_index, length=length)
  endwhile

  print, stack

  level = ''
  results = self->_import_expr(stack, 0, tree=tree, level=level)

  help, results

  obj_destroy, stack
end


;= widget events

pro comp_db_query::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'tlb':
    'add': begin
        parent = widget_info(self.current_tree_node, /parent)
        new_node = widget_tree(parent, value='AND', /folder, uvalue={type:0L}, /expanded)
      end
    'remove': begin
        widget_control, self.current_tree_node, /destroy
      end
    'child': begin
        child_node = widget_tree(self.current_tree_node, value='AND', /folder, uvalue={type:0L}, /expanded)
      end
    'tree':
    'and': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          widget_control, self.current_tree_node, set_value='AND', set_uvalue={type:0L}
        endif
      end
    'or': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          widget_control, self.current_tree_node, set_value='OR', set_uvalue={type:1L}
        endif
      end
    'condition': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          widget_control, self.current_tree_node, $
                          set_value='CONDITION', $
                          set_uvalue={type:2L, index:'', op:'', value:''}
          condition_base = widget_info(self.tlb, find_by_uname='condition_base')
          widget_control, condition_base, map=1
        endif
        self->_set_tree_buttons
        self->_set_condition_title
      end
    'field': begin
        self->_set_condition_title
      end
    'op': begin
        self->_set_condition_title
      end
    'value': begin
        self->_set_condition_title
      end
    'export': begin
        if (obj_valid(self.callback)) then begin
          callback = self.callback   ; IDL's weird syntax requires a temp var
          query = callback(self->export())
        endif
      end
    'save': begin
        filename = dialog_pickfile(/write, dialog_parent=self.tlb)
        if (filename ne '') then begin
          openw, lun, filename, /get_lun
          printf, lun, self->export()
          free_lun, lun
        endif
      end
    'open': begin
        filename = dialog_pickfile(/read, dialog_parent=self.tlb)
        if (filename ne '') then begin
          query = ''
          openr, lun, filename, /get_lun
          readf, lun, query
          free_lun, lun
          self->import, query
        endif
      end
    else: begin
        if (widget_info(event.id, /type) eq 11) then begin ; 11 = tree
          self.current_tree_node = event.id
          self->_set_tree_buttons
          widget_control, event.id, get_uvalue=uvalue
          case uvalue.type of
            0: b_uname = 'and'
            1: b_uname = 'or'
            2: b_uname = 'condition'
          endcase
          widget_control, widget_info(self.tlb, find_by_uname=b_uname), /set_button
          condition_base = widget_info(self.tlb, find_by_uname='condition_base')
          widget_control, condition_base, map=uvalue.type eq 2

          if (uvalue.type eq 2) then begin
            field_combobox = widget_info(self.tlb, find_by_uname='field')
            op_combobox = widget_info(self.tlb, find_by_uname='op')
            value_text = widget_info(self.tlb, find_by_uname='value')

            widget_control, field_combobox, set_combobox_select=self->_get_field_index(uvalue.field)
            widget_control, op_combobox, set_combobox_select=self->_get_op_index(uvalue.op)
            widget_control, value_text, set_value=uvalue.value
          endif
        endif
      end
  endcase
end


;= widget lifecycle methods

;+
; Handle cleanup when the widget program is destroyed.
;-
pro comp_db_query::cleanup_widgets
  compile_opt strictarr

  obj_destroy, self
end


pro comp_db_query::create_widgets
  compile_opt strictarr

  tree_xsize = 400.0
  clause_xsize = 300.0
  space = 5.0
  xpad = 2.0

  bitmapdir = ['resource', 'bitmaps']

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb', xpad=xpad)
  content_base = widget_base(self.tlb, xpad=0.0, ypad=0.0, space=space, /row)

  left_column = widget_base(content_base, xpad=0.0, ypad=0.0, /column)

  toolbar = widget_base(left_column, /row, /toolbar, space=5.0)
  create_toolbar = widget_base(toolbar, /row, space=0.0, xpad=0.0, ypad=0.0, /toolbar)
  add_button = widget_button(create_toolbar, /bitmap, uname='add', $
                             tooltip='Add clause', $
                             value=filepath('plus.bmp', subdir=bitmapdir))
  minus_button = widget_button(create_toolbar, /bitmap, uname='remove', $
                               tooltip='Remove clause', $
                               value=filepath('minus.bmp', subdir=bitmapdir))
  child_button = widget_button(create_toolbar, /bitmap, uname='child', $
                               tooltip='Add child clause', $
                               value=filepath('shift_right.bmp', subdir=bitmapdir))

  file_toolbar = widget_base(toolbar, /row, space=0.0, xpad=0.0, ypad=0.0, /toolbar)
  export_button = widget_button(file_toolbar, /bitmap, uname='export', $
                                tooltip='Export query', $
                                value=filepath('export.bmp', subdir=bitmapdir))
  save_button = widget_button(file_toolbar, /bitmap, uname='save', $
                              tooltip='Save query', $
                              value=filepath('save.bmp', subdir=bitmapdir))
  open_button = widget_button(file_toolbar, /bitmap, uname='open', $
                              tooltip='Open query', $
                              value=filepath('open.bmp', subdir=bitmapdir))

  tree = widget_tree(left_column, scr_xsize=tree_xsize, uname='tree')
  root = widget_tree(tree, value='AND', uname='root', /folder, uvalue={type:0L}, /expanded)

  right_column = widget_base(content_base, xpad=0.0, ypad=0.0, /column, $
                             scr_xsize=clause_xsize)
  type_base = widget_base(right_column, /row, xpad=0.0, ypad=0.0, /exclusive, /align_center)
  and_button = widget_button(type_base, value='AND', uname='and')
  or_button = widget_button(type_base, value='OR', uname='or')
  condition_button = widget_button(type_base, value='Condition', uname='condition')

  condition_base = widget_base(right_column, xpad=0.0, ypad=0.0, /row, /align_center, $
                               map=0, uname='condition_base')
  fields_combobox = widget_combobox(condition_base, value=*self.fields, uname='field')
  op_combobox = widget_combobox(condition_base, value=self->_ops(), scr_xsize=50.0, uname='op')
  value_text = widget_text(condition_base, value='', scr_xsize=100.0, /editable, uname='value')

  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=tree_xsize + space + clause_xsize + 2 * xpad, $
                                /align_left, /sunken_frame)
end


;+
; Draw the widget hierarchy.
;-
pro comp_db_query::realize_widgets
  compile_opt strictarr

  widget_control, self.tlb, /realize
  self->_set_tree_buttons
end


;+
; Start `XMANAGER`.
;-
pro comp_db_query::start_xmanager
  compile_opt strictarr

  xmanager, 'comp_db_query', self.tlb, /no_block, $
            event_handler='comp_db_query_handleevents', $
            cleanup='comp_db_query_cleanup'
end


;= lifecycle methods

pro comp_db_query::cleanup
  compile_opt strictarr

  ptr_free, self.fields
end


function comp_db_query::init, fields=fields, callback=callback
  compile_opt strictarr

  self.title = 'Database query creator'

  self.fields = ptr_new(fields)
  self.callback = callback

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  self->set_status, 'Ready.'

  return, 1
end


pro comp_db_query__define
  compile_opt strictarr

  define = { comp_db_query, $
             title: '', $
             tlb: 0L, $
             statusbar: 0L, $
             current_tree_node: 0L, $
             fields: ptr_new(), $
             callback: obj_new() $
           }
end


pro comp_db_query, fields=fields, callback=callback
  compile_opt strictarr

  query_creator = obj_new('comp_db_query', fields=fields, callback=callback)
end


; main-level example

comp_db_query, fields=['date_obs', 'rsun', 'waveleng'], callback=obj_new()

end
