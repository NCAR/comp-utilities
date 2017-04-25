; docformat = 'rst'

;+
; Browse through the MLSO database.
;-


;= helper routines

;+
; Thin procedural wrapper to call `::handle_events` event handler.
;
; :Params:
;   event : in, required, type=structure
;     event structure for event handler to handle
;-
pro comp_db_browser_handleevents, event
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
pro comp_db_browser_cleanup, tlb
  compile_opt strictarr

  widget_control, tlb, get_uvalue=browser
  browser->cleanup_widgets
end


;+
; Take a string representing a date and return it in the form YYYY-MM-DD.
;
; :Returns:
;   string
;
; :Params:
;   date : in, required, type=string
;     string representing a date in the form YYYYMMDD or YYYY-MM-DD
;-
function comp_db_browser_normalizedate, date
  compile_opt strictarr

  if (date eq '') then return, ''

  case 1 of
    stregex(date, '[[:digit:]]{8}', /boolean): begin
        return, string(strmid(date, 0, 4), $
                       strmid(date, 4, 2), $
                       strmid(date, 6, 2), $
                       format='(%"%s-%s-%s")')
      end
    stregex(date, '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}', /boolean): return, date
  endcase
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
pro comp_db_browser::set_title, filename
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
pro comp_db_browser::set_status, msg, clear=clear
  compile_opt strictarr

  _msg = keyword_set(clear) || n_elements(msg) eq 0L ? '' : msg
  widget_control, self.statusbar, set_value=_msg
end


pro comp_db_browser::_update_table, db_values, field_names
  compile_opt strictarr

  if (n_elements(db_values) eq 0L) then begin
    n_rows = 0
    _field_names = n_elements(field_names) eq 0L ? strarr(8) : field_names
    widget_control, self.table, $
                    ysize=n_rows, $
                    column_labels=_field_names
  endif else begin
    widget_control, self.table, $
                    set_value=db_values, $
                    xsize=n_tags(db_values), $
                    ysize=n_elements(db_values), $
                    column_labels=field_names
  endelse
end


function comp_db_browser::get_data, limit=limit, fields=fields, field_names=field_names
  compile_opt strictarr

  self.db->getProperty, connected=connected
  if (~connected) then begin
    self->set_status, 'Not connected to database'
    return, !null
  endif

  limit_present = n_elements(limit) gt 0L || self.current_limit gt 0
  _limit = n_elements(limit) gt 0L ? limit : self.current_limit

  self.current_table = string(self.current_type eq 'sgs' $
                                ? 'mlso' $
                                : self.current_instrument, $
                              self.current_type, $
                              format='(%"%s_%s")')

  if (self.current_table eq 'comp_sci') then begin
    self->set_status, 'No science table for CoMP'
    return, !null
  endif

  if (self.current_query ne '') then begin
    where_clause = 'where ' + self.current_query
  endif else begin
    start_date = comp_db_browser_normalizedate(self.current_start_date)
    end_date = comp_db_browser_normalizedate(self.current_end_date)
    case 1 of
      start_date ne '' && end_date ne '': begin
          where_clause = string(self.current_table, $
                                start_date, end_date, $
                                format='(%"WHERE %s.obs_day=mlso_numfiles.day_id AND mlso_numfiles.obs_day BETWEEN ''%s'' AND ''%s''")')
        end
      start_date ne '': begin
          where_clause = string(self.current_table, $
                                start_date, $
                                format='(%"WHERE %s.obs_day=mlso_numfiles.day_id AND mlso_numfiles.obs_day >= ''%s''")')
        end
      end_date ne '': begin
          where_clause = string(self.current_table, $
                                end_date, $
                                format='(%"WHERE %s.obs_day=mlso_numfiles.day_id AND mlso_numfiles.obs_day <= ''%s''")')
        end
      else: where_clause = ''
    endcase
  endelse

  field_result = self.db->query('describe %s', self.current_table, $
                                sql_statement=sql_statement, error=error, fields=fields)
  field_names = field_result.field

  query = string(self.current_table, self.current_table, where_clause, $
                 limit_present ? ' limit' : '', $
                 limit_present ? (' ' + strtrim(_limit, 2)) : '', $
                 format='(%"select %s.* from %s, mlso_numfiles %s%s%s")')
  self->set_status, string(strtrim(query, 2), format='(%"Querying with ''%s''")')
  result = self.db->query(query, $
                          sql_statement=sql_statement, error=error, fields=fields)

  if (strlowcase(error) ne 'success') then begin
    self->set_status, string(sql_statement, $
                             format='(%"Problem with SQL statement: ''%s''")')
  endif else begin
    if (n_elements(result) eq 0L) then begin
      *self.fields = !null
    endif else begin
      *self.fields = fields
    endelse
    self->set_status, string(n_elements(result), $
                             strtrim(sql_statement, 2), $
                             format='(%"%d results for query: ''%s''")')
  endelse

  return, result
end


;= widget events

pro comp_db_browser::handle_events, event
  compile_opt strictarr

  uname = widget_info(event.id, /uname)
  case uname of
    'tlb': begin
        tlb_geometry = widget_info(self.tlb, /geometry)
        table_geometry = widget_info(self.table, /geometry)
        statusbar_geometry = widget_info(self.statusbar, /geometry)

        table_width = event.x $
                        - 2 * tlb_geometry.xpad $
                        - 3
        statusbar_width = table_width
        height = event.y - 3 * tlb_geometry.ypad $
                   - statusbar_geometry.scr_ysize $
                   - 2 * statusbar_geometry.margin

        widget_control, self.tlb, update=0

        widget_control, self.table, scr_xsize=table_width, scr_ysize=height
        widget_control, self.statusbar, scr_xsize=statusbar_width

        widget_control, self.tlb, update=1
      end
    'comp': begin
        self.current_instrument = 'comp'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'kcor': begin
        self.current_instrument = 'kcor'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'images': begin
        self.current_type = 'img'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'eng': begin
        self.current_type = 'eng'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'cal': begin
        self.current_type = 'cal'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'sci': begin
        self.current_type = 'sci'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'sgs': begin
        self.current_type = 'sgs'
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'start_date': begin
        widget_control, event.id, get_value=start_date
        self.current_start_date = start_date
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'end_date': begin
        widget_control, event.id, get_value=end_date
        self.current_end_date = end_date
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'limit': begin
        widget_control, event.id, get_value=limit_value
        self.current_limit = limit_value eq '' ? -1L : long(limit_value)
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'create_query': begin
        result = self->get_data(limit=1, fields=fields)
        if (n_elements(result) gt 0L) then begin
          comp_db_query, fields=fields.name, callback=self
        endif
      end
    'clear_query': begin
        self.current_query = ''
        self->_update_table, self->get_data(field_names=field_names), field_names
      end
    'cmdline': begin
        widget_control, self.table, get_value=data
        geometry = widget_info(self.table, /geometry)
        if (geometry.xsize eq 0L || geometry.ysize eq 0L) then data = !null
        (scope_varfetch('data', /enter, level=1)) = data
      end
    'plot': begin
        widget_control, self.table, get_value=data
        if (n_elements(*self.fields) gt 0L) then begin
          comp_db_plot, self.current_table, fields=*self.fields, data=data
        endif
      end
    else:
  endcase
end


;= widget lifecycle methods

;+
; Handle cleanup when the widget program is destroyed.
;-
pro comp_db_browser::cleanup_widgets
  compile_opt strictarr

  obj_destroy, self
end


pro comp_db_browser::create_widgets
  compile_opt strictarr

  table_xsize = 1050
  table_ysize = 600
  xpad = 0

  self.tlb = widget_base(title=self.title, /column, /tlb_size_events, $
                         uvalue=self, uname='tlb')

  ; toolbar
  bitmapdir = ['resource', 'bitmaps']
  space = 10.0
  toolbar = widget_base(self.tlb, /row, uname='toolbar', $
                        /base_align_center, space=0.0)

  instrument_toolbar = widget_base(toolbar, frame=1, xpad=0.0, ypad=0.0, /row)
  instrument_label = widget_label(instrument_toolbar, value='Instrument:')
  instrument_base = widget_base(instrument_toolbar, xpad=0.0, ypad=0.0, /exclusive, /row)
  comp_button = widget_button(instrument_base, value='CoMP', uname='comp')
  kcor_button = widget_button(instrument_base, value='KCor', uname='kcor')
  widget_control, comp_button, /set_button

  spacer = widget_base(toolbar, scr_xsize=space, xpad=0.0, ypad=0.0)

  type_toolbar = widget_base(toolbar, frame=1, xpad=0.0, ypad=0.0, /row)
  type_label = widget_label(type_toolbar, value='Type:')
  type_base = widget_base(type_toolbar, xpad=0.0, ypad=0.0, /exclusive, /row)
  images_button = widget_button(type_base, value='images', uname='images')
  widget_control, images_button, /set_button
  engineering_button = widget_button(type_base, value='engineering', uname='eng')
  cal_button = widget_button(type_base, value='calibration', uname='cal')
  sci_button = widget_button(type_base, value='science', uname='sci')
  sgs_button = widget_button(type_base, value='SGS', uname='sgs')

  spacer = widget_base(toolbar, scr_xsize=space, xpad=0.0, ypad=0.0)
  dates_label = widget_label(toolbar, value='Dates:')
  start_text = widget_text(toolbar, value='', uname='start_date', $
                           scr_xsize=70.0, ysize=1, $
                           /editable)
  to_label = widget_label(toolbar, value='to')
  end_text = widget_text(toolbar, value='', uname='end_date', $
                           scr_xsize=70.0, ysize=1, $
                           /editable)

  spacer = widget_base(toolbar, scr_xsize=space, xpad=0.0, ypad=0.0)

  limit_label = widget_label(toolbar, value='Limit:')
  limit_text = widget_text(toolbar, value='500', uname='limit', $
                           scr_xsize=60.0, ysize=1, $
                           /editable)

  spacer = widget_base(toolbar, scr_xsize=space, xpad=0.0, ypad=0.0)

  query_button = widget_button(toolbar, /bitmap, uname='create_query', $
                              tooltip='Create query', $
                              value=filepath('find.bmp', subdir=bitmapdir))
  clear_query_button = widget_button(toolbar, /bitmap, uname='clear_query', $
                              tooltip='Clear query', $
                              value=filepath('delete.bmp', subdir=bitmapdir))

  spacer = widget_base(toolbar, scr_xsize=space, xpad=0.0, ypad=0.0)

  cmdline_button = widget_button(toolbar, /bitmap, uname='cmdline', $
                                 tooltip='Export to command line', $
                                 value=filepath('commandline.bmp', $
                                                subdir=bitmapdir))
  plot_button = widget_button(toolbar, /bitmap, uname='plot', $
                              tooltip='Plot', $
                              value=filepath('plot.bmp', subdir=bitmapdir))

  self.current_table = 'comp_img'
  self.current_instrument = 'comp'
  self.current_type = 'img'

  n_rows = 0L
  n_columns = 10L

  self.table = widget_table(self.tlb, $
                            /no_row_headers, $
                            background_color=bytarr(3) + 210B, $
                            ;column_labels=tag_names(db_values[0]), $
                            ;value=db_values, $
                            ;xsize=n_tags(db_values[0]), $
                            xsize=n_columns, $
                            ysize=n_rows, $
                            scr_xsize=table_xsize, $
                            scr_ysize=table_ysize, $
                            uname='table', $
                            /resizeable_columns, $
                            /all_events, $
                            /context_events)
  self.statusbar = widget_label(self.tlb, $
                                scr_xsize=table_xsize + 2 * xpad, $
                                /align_left, /sunken_frame)
end


;+
; Draw the widget hierarchy.
;-
pro comp_db_browser::realize_widgets
  compile_opt strictarr

  widget_control, self.tlb, /realize
end


;+
; Start `XMANAGER`.
;-
pro comp_db_browser::start_xmanager
  compile_opt strictarr

  xmanager, 'comp_db_browser', self.tlb, /no_block, $
            event_handler='comp_db_browser_handleevents', $
            cleanup='comp_db_browser_cleanup'
end


;= IDL_Object methods

function comp_db_browser::_overloadFunction, query
  compile_opt strictarr

  self.current_query = query
  self->_update_table, self->get_data(field_names=field_names), field_names

  return, query
end


;= property access

pro comp_db_browser::setProperty, database=database, table=table
  compile_opt strictarr

  if (n_elements(database) gt 0L) then begin
    self.current_database = database
    self.db->setProperty, database=database
  endif

  if (n_elements(table) gt 0L) then begin
    self.current_table = table
  endif
end



;= lifecycle methods

pro comp_db_browser::cleanup
  compile_opt strictarr

  obj_destroy, self.db
  ptr_free, self.fields
end


function comp_db_browser::init, config_filename, section=section
  compile_opt strictarr

  self.title = 'MLSO database browser'

  _config_filename = n_elements(config_filename) eq 0L $
                       ? filepath('.mysqldb', root=getenv('HOME')) $
                       : config_filename

  config = mg_read_config(_config_filename)

  config->getProperty, sections=sections

  _section = n_elements(section) eq 0L ? sections[0] : section

  obj_destroy, config

  self.fields = ptr_new(/allocate_heap)
  self.current_limit = 500
  self.current_type = 'img'

  self.db = mgdbmysql()

  self.db->connect, config_filename=_config_filename, $
                    config_section=_section, $
                    error_message=error_message
  self.db->getProperty, host_name=host, connected=connected

  self->create_widgets
  self->realize_widgets
  self->start_xmanager

  msg = connected $
          ? string(host, format='(%"Connected to %s...\n")') $
          : string(error_message, format='(%"Error connecting to database: %s")')
  self->set_status, msg

  self->_update_table, self->get_data(field_names=field_names), field_names

  return, 1
end


pro comp_db_browser__define
  compile_opt strictarr

  define = { comp_db_browser, inherits IDL_Object, $
             title: '', $
             tlb: 0L, $
             db: obj_new(), $
             fields: ptr_new(), $
             table: 0L, $
             statusbar: 0L, $
             current_database: '', $
             current_table: '', $
             current_start_date: '', $
             current_end_date: '', $
             current_limit: 0L, $
             current_instrument: '', $
             current_type: '', $
             current_query: '' $
           }
end


;+
; Browse the CoMP data in the MLSO database.
;
; :Params:
;   config_filename : in, optional, type=string, default=~/.mysqldb
;     configuration file with login information for database
;
; :Keywords:
;   section : in, optional, type=string
;     section of the configuration file to use; defaults to the first
;     section
;-
pro comp_db_browser, config_filename, section=section
  compile_opt strictarr
  on_error, 2

  browser = obj_new('comp_db_browser', config_filename, section=section)
end
