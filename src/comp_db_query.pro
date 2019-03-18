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


pro comp_db_query::_set_type_controls, uvalue
  compile_opt strictarr

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


function comp_db_query::_loadicon, basename, read=read
  compile_opt strictarr

  filename = filepath(basename, subdir=['resource', 'bitmaps'])
  if (keyword_set(read)) then begin
    im = read_bmp(filename, r, g, b)
    im3 = make_array(dimension=[size(im, /dimensions), 3], type=size(im, /type))
    im3[*, *, 0] = r[im]
    im3[*, *, 1] = g[im]
    im3[*, *, 2] = b[im]
    return, im3
  endif else begin
    return, filename
  endelse
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


function comp_db_query::_import_condition, stack, pos, level=level
  compile_opt strictarr

  field = stack[pos++]
  ops = self->_ops()
  ind = where(stack[pos++] eq ops, count)
  if (count eq 0) then message, 'expecting operator'
  op = ops[ind[0]]
  value = stack[pos++]
  value = strmid(value, 1, strlen(value) - 2)
  condition = {type:2, field:field, op:op, value:value}
  return, condition
end


function comp_db_query::_import_expr, stack, pos, level=level
  compile_opt strictarr

  if (stack[pos] eq '(') then begin
    type = ''
    expressions = list()

    done = 0B
    while (~done) do begin
      pos++   ; consume (
      expr = self->_import_expr(stack, pos, level=level + '  ')
      expressions->add, expr

      if (stack[pos] ne ')') then message, 'expected close parenthesis'
      pos++   ; consume )

      done = pos ge stack->count() || (stack[pos] ne 'and' && stack[pos] ne 'or')
      if (~done) then begin
        if (type ne '' && stack[pos] ne type) then message, 'inconsistent expression'
        type = stack[pos]
        pos++
      endif
    endwhile

    return, {type: type eq 'and' ? 0L : 1L, expressions: expressions}
  endif else begin
    return, self->_import_condition(stack, pos, level=level + '  ')
  endelse
end


pro comp_db_query::_import_results, results, tree=tree, top=top
  compile_opt strictarr

  if (results.type eq 2L) then begin
    value = string(results.field, results.op, results.value, format='(%"%s %s ''%s''")')
    uvalue = results
    parent = widget_tree(tree, value=value, uvalue=uvalue, $
                         bitmap=self->_loadicon('new.bmp', /read))
  endif else begin
    uvalue = {type: results.type}
    parent = widget_tree(tree, $
                         value=results.type eq 0 ? 'and' : 'or', $
                         uvalue=uvalue, $
                         /folder, /expanded, $
                         bitmap=self->_loadicon('mcr.bmp', /read))
    foreach e, results.expressions do begin
      self->_import_results, e, tree=parent
    endforeach
  endelse

  if (keyword_set(top)) then begin
    self.current_tree_node = parent
    widget_control, self.current_tree_node, set_tree_select=1, set_uname='root'
    self->_set_type_controls, uvalue
  endif
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

  level = ''
  results = self->_import_expr(stack, 0, level=level)
  widget_control, tree, update=0
  self->_import_results, results, tree=tree, /top
  widget_control, tree, update=1
  self->_set_tree_buttons

  obj_destroy, stack
end


;= widget events

pro comp_db_query::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'tlb': begin
        toolbar = widget_info(self.tlb, find_by_uname='toolbar')
        tree = widget_info(self.tlb, find_by_uname='tree')
        bottom = widget_info(self.tlb, find_by_uname='bottom')

        tlb_geometry = widget_info(self.tlb, /geometry)
        toolbar_geometry = widget_info(toolbar, /geometry)
        bottom_geometry = widget_info(bottom, /geometry)
        statusbar_geometry = widget_info(self.statusbar, /geometry)

        _x = event.x > 500.0
        _y = event.y > 200.0

        widget_control, self.tlb, update=0
        widget_control, self.statusbar, scr_xsize=_x - 2 * tlb_geometry.xpad
        widget_control, tree, scr_xsize=_x - 2 * tlb_geometry.xpad, $
                              scr_ysize=_y $
                                - 2 * tlb_geometry.ypad $
                                - 3 * tlb_geometry.space $
                                - toolbar_geometry.scr_ysize $
                                - bottom_geometry.scr_ysize $
                                - statusbar_geometry.scr_ysize
        widget_control, bottom, scr_xsize=_x - 2 * tlb_geometry.xpad, $
                                scr_ysize=bottom_geometry.scr_ysize
        widget_control, self.tlb, update=1
      end
    'add': begin
        parent = widget_info(self.current_tree_node, /parent)
        new_node = widget_tree(parent, $;value='and', $
                               ;/folder, /expanded, $
                               uvalue={type:2L}, $
                               bitmap=self->_loadicon('new.bmp', /read))
        self.current_tree_node = new_node
        widget_control, new_node, /set_tree_select
        self->_set_tree_buttons
        self->_set_condition_title
        widget_control, new_node, get_uvalue=uvalue
        self->_set_type_controls, uvalue
      end
    'remove': begin
        widget_control, self.current_tree_node, /destroy
      end
    'child': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          ctn = self.current_tree_node
        endif else begin
          ctn = widget_info(self.tlb, find_by_uname='tree')
        endelse
        child_node = widget_tree(ctn, $;value='and', /folder, /expanded, $
                                 uvalue={type:2L}, $
                                 bitmap=self->_loadicon('new.bmp', /read))
        self.current_tree_node = child_node
        widget_control, child_node, /set_tree_select
        self->_set_tree_buttons
        self->_set_condition_title
        widget_control, child_node, get_uvalue=uvalue
        self->_set_type_controls, uvalue
      end
    'tree':
    'and': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          parent = widget_info(self.current_tree_node, /parent)
          uname = widget_info(self.current_tree_node, /uname)
          widget_control, self.current_tree_node, /destroy
          self.current_tree_node = widget_tree(parent, $
                                               value='and', $
                                               /folder, /expanded, $
                                               uname=uname, $
                                               uvalue={type:0L}, $
                                               bitmap=self->_loadicon('mcr.bmp', /read))
          condition_base = widget_info(self.tlb, find_by_uname='condition_base')
          widget_control, self.current_tree_node, set_tree_select=1
          widget_control, condition_base, map=0
        endif
      end
    'or': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          parent = widget_info(self.current_tree_node, /parent)
          uname = widget_info(self.current_tree_node, /uname)
          widget_control, self.current_tree_node, /destroy
          self.current_tree_node = widget_tree(parent, $
                                               value='or', $
                                               /folder, /expanded, $
                                               uname=uname, $
                                               uvalue={type:1L}, $
                                               bitmap=self->_loadicon('mcr.bmp', /read))
          condition_base = widget_info(self.tlb, find_by_uname='condition_base')
          widget_control, self.current_tree_node, set_tree_select=1
          widget_control, condition_base, map=0
        endif
      end
    'condition': begin
        if (widget_info(self.current_tree_node, /valid)) then begin
          parent = widget_info(self.current_tree_node, /parent)
          uname = widget_info(self.current_tree_node, /uname)
          widget_control, self.current_tree_node, /destroy
          self.current_tree_node = widget_tree(parent, $
                                               value='CONDITION', $
                                               uname=uname, $
                                               uvalue={type:2L, index:'', op:'', value:''}, $
                                               bitmap=self->_loadicon('new.bmp', /read))
          condition_base = widget_info(self.tlb, find_by_uname='condition_base')
          widget_control, self.current_tree_node, set_tree_select=1
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
        case 1 of
          file_test(filename, /directory): begin
              ok = dialog_message('Directory selected', dialog_parent=self.tlb)
            end
          filename eq '': begin
              ok = dialog_message('No file selected', dialog_parent=self.tlb)
            end
          else: begin
              openw, lun, filename, /get_lun
              printf, lun, self->export()
              free_lun, lun
            end
        endcase
      end
    'open': begin
        filename = dialog_pickfile(/read, dialog_parent=self.tlb)
        if (filename ne '') then begin
          query = ''
          openr, lun, filename, /get_lun
          readf, lun, query
          free_lun, lun
          self->import, query
          self->set_status, 'Loaded query from ' + filename
        endif
      end
    else: begin
        if (widget_info(event.id, /type) eq 11) then begin ; 11 = tree
          self.current_tree_node = event.id
          self->_set_tree_buttons
          widget_control, event.id, get_uvalue=uvalue
          self->_set_type_controls, uvalue
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


pro comp_db_query::create_widgets, group_leader=group_leader
  compile_opt strictarr

  tree_xsize = 500.0
  tree_ysize = 200.0
  space = 5.0
  xpad = 2.0

  if (n_elements(group_leader) gt 0L) then begin
    geometry = widget_info(group_leader, /geometry)
    xoffset = geometry.xoffset + 100.0
    yoffset = geometry.yoffset + 100.0
  endif

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb', xpad=xpad, $
                         group_leader=group_leader, xoffset=xoffset, yoffset=yoffset)
  content_base = widget_base(self.tlb, xpad=0.0, ypad=0.0, space=space, /column)

  top_column = widget_base(content_base, xpad=0.0, ypad=0.0, /column)

  toolbar = widget_base(top_column, /row, /toolbar, space=5.0, uname='toolbar')
  create_toolbar = widget_base(toolbar, /row, space=0.0, xpad=0.0, ypad=0.0, /toolbar)
  add_button = widget_button(create_toolbar, /bitmap, uname='add', $
                             tooltip='Add clause', $
                             value=self->_loadicon('plus.bmp'))
  minus_button = widget_button(create_toolbar, /bitmap, uname='remove', $
                               tooltip='Remove clause', $
                               value=self->_loadicon('minus.bmp'))
  child_button = widget_button(create_toolbar, /bitmap, uname='child', $
                               tooltip='Add child clause', $
                               value=self->_loadicon('shift_right.bmp'))

  file_toolbar = widget_base(toolbar, /row, space=0.0, xpad=0.0, ypad=0.0, /toolbar)
  export_button = widget_button(file_toolbar, /bitmap, uname='export', $
                                tooltip='Export query', $
                                value=self->_loadicon('export.bmp'))
  save_button = widget_button(file_toolbar, /bitmap, uname='save', $
                              tooltip='Save query', $
                              value=self->_loadicon('save.bmp'))
  open_button = widget_button(file_toolbar, /bitmap, uname='open', $
                              tooltip='Open query', $
                              value=self->_loadicon('open.bmp'))

  tree = widget_tree(top_column, scr_xsize=tree_xsize, scr_ysize=tree_ysize, uname='tree')
  root = widget_tree(tree, value='and', uname='root', /folder, /expanded, $
                     uvalue={type:0L}, $
                     bitmap=self->_loadicon('mcr.bmp', /read))

  bottom_column = widget_base(content_base, xpad=0.0, ypad=0.0, /column, $
                              scr_xsize=tree_xsize, uname='bottom', /frame)
  type_base = widget_base(bottom_column, /row, xpad=0.0, ypad=0.0, /exclusive, /align_center)
  and_button = widget_button(type_base, value='and', uname='and')
  or_button = widget_button(type_base, value='or', uname='or')
  condition_button = widget_button(type_base, value='condition', uname='condition')

  condition_base = widget_base(bottom_column, xpad=0.0, ypad=0.0, /row, /align_center, $
                               map=0, uname='condition_base')
  fields_combobox = widget_combobox(condition_base, value=*self.fields, uname='field')
  op_combobox = widget_combobox(condition_base, value=self->_ops(), scr_xsize=50.0, uname='op')
  value_text = widget_text(condition_base, value='', scr_xsize=300.0, /editable, uname='value')

  spacer = widget_base(bottom_column, scr_ysize=10.0)

  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=tree_xsize + 2 * xpad, $
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


function comp_db_query::init, fields=fields, $
                              callback=callback, $
                              group_leader=group_leader
  compile_opt strictarr

  self.title = 'Database query creator'

  self.fields = ptr_new(fields)
  self.callback = callback

  self->create_widgets, group_leader=group_leader
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


pro comp_db_query, fields=fields, callback=callback, group_leader=group_leader
  compile_opt strictarr

  query_creator = obj_new('comp_db_query', $
                          fields=fields, $
                          callback=callback, $
                          group_leader=group_leader)
end


; main-level example

comp_db_query, fields=['date_obs', 'rsun', 'waveleng'], callback=obj_new()

end
