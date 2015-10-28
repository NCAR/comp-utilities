; docformat = 'rst'

;= lifecycle methods

pro comp_db_browser::cleanup
  compile_opt strictarr

  obj_destroy, self.db
end


function comp_db_browser::init, config_filename, section=section
  compile_opt strictarr

  _config_filename = n_elements(config_filename) eq 0L $
                       ? filepath('.mysqldb', root=getenv('HOME')) $
                       : config_filename

  config = mg_read_config(_config_filename)

  config->getProperty, sections=sections

  _section = n_elements(section) eq 0L ? sections[0] : section

  obj_destroy, config

  self.db = mgdbmysql()
  self.db->connect, config_filename=_config_filename, $
                    config_section=_section, $
                    database='MLSO'

  return, 1
end


pro comp_db_browser__define
  compile_opt strictarr

  define = { comp_db_browser, $
             db: obj_new() $
           }
end


;+
; Browse the CoMP data in the MLSO database.
;
; :Params:
;   config_filename : in, optional, type=string, default=~/.mysqldb
;     configuration file with login information for database
;   section : in, optional, type=string
;     section of the configuration file to use; defaults to the first
;     section
;-
pro comp_db_browser, config_filename, section=section
  compile_opt strictarr
  on_error, 2

  browser = obj_new('comp_db_browser', config_filename, section=section)
end
