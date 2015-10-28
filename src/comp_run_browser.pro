; docformat = 'rst'

;+
; Launch various browsers for a given configuration file.
;
; :Keywords:
;   config_filename : in, required, type=string
;     filename of CoMP configuration file
;-
pro comp_run_browser, config_filename
  compile_opt strictarr

  config = mg_read_config(config_filename)

  process_basedir  = config->get('process_basedir', section='processing')
  raw_basedir      = config->get('raw_basedir', section='processing')

  comp_dir_browser, [process_basedir, raw_basedir]

  obj_destroy, config
end
