; docformat = 'rst'

;+
; GUI browser for CoMP data.
;
; .. image:: browser.png
;-

;= helper methods

;+
; Determine CoMP wave type from an exact wavelength.
;
; :Private:
;
; :Returns:
;   string: '1074', '1079', or '1083'
;
; :Params:
;   wavelength : in, required, type=float
;     exact wavelength
;-
function comp_browser::get_wave_type, wavelength
  compile_opt strictarr

  ; find nearest wave type to the exact wavelength
  wave_types = ['1074', '1079', '1083']
  !null = min(abs(float(wave_types) - wavelength), index)
  return, wave_types[index]
end


;+
; Determine whether the data is raw or level 1.
;
; :Returns:
;   0 (raw), 1 (level 1), or -1 (unknown)
;
; :Params:
;   data : in, required, type=fltarr
;     data read from FITS file
;   header : in, required, type=strarr
;     FITS header
;-
function comp_browser::get_level, data, header
  compile_opt strictarr

  ndims = size(data, /n_dimensions)
  if (ndims ne 2) then return, -1

  dims = size(data, /dimensions)
  if (array_equal(dims, [1024, 1024])) then return, 0
  if (array_equal(dims, [620, 620])) then return, 1

  return, -1
end


;= CoMP specific overrides from MG_FITS_Browser

;+
; Return title to display for extension.
;
; :Returns:
;   string
;
; :Params:
;   filename : in, required, type=string
;     filename of FITS file
;   header : in, required, type=strarr
;     primary header of FITS file
;-
function comp_browser::file_title, filename, header
  compile_opt strictarr

  return, file_basename(filename)
end


;+
; Make a solid colored icon.
;
; :Returns:
;   bytarr(16, 16, 3)
;
; :Params:
;   color : in, required, type=bytarr(3)
;     color to make icon
;-
function comp_browser::_make_icon, color
  compile_opt strictarr

  bmp = bytarr(16, 16, 3)
  bmp[*, *, 0] = color[0]
  bmp[*, *, 1] = color[1]
  bmp[*, *, 2] = color[2]

  return, bmp
end


;+
; Return bitmap of icon to display next to the file.
;
; :Returns:
;   `bytarr(m, n, 3)` or `bytarr(m, n, 4)` or `0` if default is to be used
;
; :Params:
;   filename : in, required, type=string
;     filename of FITS file
;   header : in, required, type=strarr
;     primary header of FITS file
;-
function comp_browser::file_bitmap, filename, header
  compile_opt strictarr

  comp_query_file, filename, type=type
  level = strtrim(sxpar(header, 'LEVEL', count=level_found), 2)

  case type of
    'DARK': bmp = bytarr(16, 16, 3)
    'OPAL': bmp = bytarr(16, 16, 3) + 128B
    'CALIBRATION': begin
        bmp = read_png(filepath('geardata24.png', $
                                subdir=['resource', 'bitmaps']))
        bmp = transpose(bmp, [1, 2, 0])
        bmp = congrid(bmp, 16, 16, 4)
      end
    'DATA': begin
        if (level_found && level eq 'L1') then begin
          bmp = read_png(filepath('level1.png', root=mg_src_root()))
          bmp = transpose(bmp, [1, 2, 0])
        endif else begin
          bmp = read_png(filepath('raw.png', root=mg_src_root()))
          bmp = transpose(bmp, [1, 2, 0])
        endelse
      end
    'UNKNOWN': bmp = bytarr(16, 16, 3) + 255B
    'DYNAMICS': bmp = self->_make_icon([255B, 200B, 200B])
    'POLARIZATION': bmp = self->_make_icon([255B, 220B, 220B])
    'MEAN': bmp = self->_make_icon([230B, 255B, 230B])
    'MEDIAN': bmp = self->_make_icon([230B, 255B, 230B])
    'QUICK_INVERT': bmp = self->_make_icon([230B, 255B, 230B])
    'SIGMA': bmp = self->_make_icon([230B, 255B, 230B])
    else: bmp = bytarr(16, 16, 3) + 255B
  endcase

  return, bmp
end


;+
; Return title to display for extension.
;
; :Returns:
;   string
;
; :Params:
;   n_exts : in, required, type=long
;     number of extensions
;   ext_names : in, required, type=strarr
;     extension names
;
; :Keywords:
;   filename : in, required, type=string
;     filename of file
;-
function comp_browser::extension_title, n_exts, ext_names, filename=filename
  compile_opt strictarr

  comp_query_file, filename, $
                   type=type, $
                   beam_state=beam_state, $
                   wavelength=wavelength, $
                   polarization_state=polarization_state, $
                   exposures=exposures

  titles = strarr(n_exts)

  case type of
    'DARK': begin
        if (file_basename(filename) eq 'dark.fts') then begin   ; level 1
          fits_open, filename, fcb
          for e = 1L, n_exts do begin
            if (ext_names[e] ne '') then begin
              titles[e - 1] = ext_names[e]
            endif else begin
              fits_read, fcb, data, header, exten_no=e, /header_only
              titles[e - 1] = string(sxpar(header, 'TIME_OBS'), $
                                     exposures[e - 1], $
                                     format='(%"%s @ %0.1f ms")')
            endelse
          endfor
          fits_close, fcb
        endif else begin   ; raw
          beam_description = 'Corona in ' + (['UL', 'LR'])[beam_state > 0]

          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   beam_description[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm (%s)")')
          endfor
        endelse
      end
    'OPAL': begin
        if (file_basename(filename) eq 'flat.fts') then begin   ; level 1
          fits_open, filename, fcb
          fits_read, fcb, times, times_header, exten_no=n_exts - 2
          fits_read, fcb, exposures, exposures_header, exten_no=n_exts

          beam_description = 'Corona in ' + (['UL', 'LR'])[beam_state > 0]

          for e = 1L, n_exts do begin
            if (e ge n_exts - 2) then begin
              titles[e - 1] = fcb.extname[e]
            endif else begin
              titles[e - 1] = string(comp_times2str(times[e - 1]), $
                                     wavelength[e - 1], $
                                     beam_description[e - 1], $
                                     format='(%"%s @ %0.2f nm (%s)")')
            endelse
          endfor

          fits_close, fcb
        endif else begin   ; raw
          beam_description = 'Corona in ' + (['UL', 'LR'])[beam_state > 0]

          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   beam_description[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm (%s)")')
          endfor
        endelse
      end
    'CALIBRATION': begin
          beam_description = 'Corona in ' + (['UL', 'LR'])[beam_state > 0]
          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   beam_description[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm (%s)")')
          endfor
        end
    'DATA': begin
        !null = where(beam_state eq 0, n_zero_beam)
        if (n_zero_beam gt 0L) then begin   ; level 1
          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm")')
          endfor
        endif else begin   ; raw
          beam_description = 'Corona in ' + (['UL', 'LR'])[beam_state > 0]

          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   beam_description[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm (%s)")')
          endfor
        endelse
      end
    'BACKGROUND': begin
          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm")')
          endfor
        end
    'MEAN': begin
          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm")')
          endfor
      end
    'MEDIAN': begin
          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm")')
          endfor
      end
    'SIGMA': begin
          for e = 1L, n_exts do begin
            titles[e - 1] = string(e, $
                                   polarization_state[e - 1], $
                                   wavelength[e - 1], $
                                   format='(%"ext %d: %s @ %0.2f nm")')
          endfor
      end
    else: titles = self->mg_fits_browser::extension_title(n_exts, ext_names)
  endcase

  return, titles
end


;+
; Return bitmap of icon to display next to the extension.
;
; :Returns:
;   `bytarr(m, n, 3)` or `bytarr(m, n, 4)` or `0` if default is to be used
;
; :Params:
;   ext_number : in, required, type=long
;     extension number
;   ext_name : in, required, type=long
;     extension name
;   ext_header : in, required, type=strarr
;     header for extension
;-
function comp_browser::extension_bitmap, ext_number, ext_name, ext_header
  compile_opt strictarr

  datatype = sxpar(ext_header, 'DATATYPE', count=count)
  if (size(datatype, /type) ne 7) then return, 0

  case datatype of
    'DATA': begin
        level = strtrim(sxpar(ext_header, 'LEVEL', count=level_found), 2)
        if (level_found && level eq 'L1') then begin
          bmp = read_png(filepath('level1.png', root=mg_src_root()))
          bmp = transpose(bmp, [1, 2, 0])
        endif else begin
          bmp = read_png(filepath('raw.png', root=mg_src_root()))
          bmp = transpose(bmp, [1, 2, 0])
        endelse
      end
    'DARK': bmp = bytarr(16, 16, 3)
    'FLAT': bmp = bytarr(16, 16, 3) + 128B
    else: bmp = 0
  endcase

  return, bmp
end


;+
; Returns valid file extensions.
;
; :Private:
;
; :Returns:
;   strarr
;-
function comp_browser::file_extensions
  compile_opt strictarr

  return, [['*.fts;*.FTS', '*.*'], $
           ['CoMP FITS files', 'All files']]
end


;+
; Display the given data as an image.
;
; :Private:
;
; :Params:
;   data : in, required, type=2D array
;     data to display
;   header : in, required, type=strarr
;     FITS header
;-
pro comp_browser::display_image, data, header
  compile_opt strictarr

  level = self->get_level(data, header)
  if (level lt 0) then begin
    self->erase
    return
  endif

  draw_wid = widget_info(self.tlb, find_by_uname='draw')
  geo_info = widget_info(draw_wid, /geometry)

  dims = size(data, /dimensions)

  data_aspect_ratio = float(dims[1]) / float(dims[0])
  draw_aspect_ratio = float(geo_info.draw_ysize) / float(geo_info.draw_xsize)

  if (data_aspect_ratio gt draw_aspect_ratio) then begin
    ; use y as limiting factor for new dimensions
    dims *= geo_info.draw_ysize / float(dims[1])
  endif else begin
    ; use x as limiting factor for new dimensions
    dims *= geo_info.draw_xsize / float(dims[0])
  endelse

  _data = congrid(data, dims[0], dims[1])

  top = 250
  case level of
    0: begin
        display_min = 0.0
        display_max = 5000.0
        power = 1.0
        ct = 0
      end
    1: begin
        wave_type = self->get_wave_type(sxpar(header, 'WAVELENG'))
        ct = 3
        case wave_type of
          '1074' : begin
              display_min = 0.0
              display_max = 5.0
              power = 0.5
            end
          '1079' : begin
              display_min = 0
              display_max = 3.5
              power = 0.5
            end
          '1083' : begin
              display_min = 0
              display_max = 12.0
              power = 0.3
            end
        endcase
      end
    else: message, 'unknown level'
  endcase

  image = bytscl((_data > 0.0)^power, min=display_min, max=display_max, top=top)

  if (dims[0] gt geo_info.draw_xsize || dims[1] gt geo_info.draw_ysize) then begin
    xoffset = 0
    yoffset = 0
  endif else begin
    xoffset = (geo_info.draw_xsize - dims[0]) / 2
    yoffset = (geo_info.draw_ysize - dims[1]) / 2
  endelse

  old_win_id = !d.window
  device, get_decomposed=odec
  tvlct, rgb, /get

  device, decomposed=0
  loadct, ct, /silent
  wset, self.draw_id
  tvscl, image, xoffset, yoffset

  tvlct, rgb
  device, decomposed=odec
  wset, old_win_id
end


;+
; Overlay information on the image.
;
; :Params:
;   data : in, required, type=2D array
;     data to display
;   header : in, required, type=strarr
;     FITS header
;-
pro comp_browser::annotate_image, data, header
  compile_opt strictarr

  ; back sure data is passed in
  if (n_elements(data) eq 0L $
        || size(data, /n_dimensions) ne 2L $
        || n_elements(header) eq 0L) then return

  dims = size(data, /dimensions)

  ; only annotating level 1 data
  if (~array_equal(dims, [620, 620])) then return

  draw_wid = widget_info(self.tlb, find_by_uname='draw')
  geo_info = widget_info(draw_wid, /geometry)

  frpix1 = (sxpar(header, 'FRPIX1') - 1.0) / dims[0]
  frpix2 = (sxpar(header, 'FRPIX2') - 1.0) / dims[1]
  fradius = (sxpar(header, 'FRADIUS'))

  crpix1 = (sxpar(header, 'CRPIX1') - 1.0) / dims[0]
  crpix2 = (sxpar(header, 'CRPIX2') - 1.0) / dims[1]
  cradius = (sxpar(header, 'ORADIUS'))

  t = findgen(360) * !dtor
  fx = geo_info.draw_xsize * (fradius / dims[0] * cos(t) + frpix1)
  fy = geo_info.draw_ysize * (fradius / dims[1] * sin(t) + frpix2)

  cx = geo_info.draw_xsize * (cradius / dims[0] * cos(t) + crpix1)
  cy = geo_info.draw_ysize * (cradius / dims[1] * sin(t) + crpix2)

  fieldstop_color = 'ffffff'x
  occulter_color = '00ffff'x

  device, get_decomposed=odec
  device, decomposed=1

  plots, geo_info.draw_xsize * [frpix1], geo_info.draw_ysize * [frpix2], $
         psym=1, /device, color=fieldstop_color
  plots, fx, fy, /device, color=fieldstop_color

  plots, geo_info.draw_xsize * [crpix1], geo_info.draw_ysize * [crpix2], $
         psym=1, /device, color=occulter_color
  plots, cx, cy, /device, color=occulter_color

  device, decomposed=odec
end


;+
; Determine if annotation is avalable for a given image.
;
; :Params:
;   data : in, required, type=2D array
;     data to display
;   header : in, required, type=strarr
;     FITS header
;-
function comp_browser::annotate_available, data, header
  compile_opt strictarr

  level = self->get_level(data, header)
  return, level ge 1L
end


;= lifecycle methods

;+
; Create a CoMP data file browser.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;  _extra : in, optional, type=keywords
;    keywords to `mg_fits_browser::init`
;-
function comp_browser::init, tlb=tlb, _extra=e
  compile_opt strictarr

  if (~self->mg_fits_browser::init(/tlb_size_events, _extra=e)) then return, 0

  tlb = self.tlb
  self.draw_size = 512.0

  return, 1
end


;+
; Define CoMP_Browser class, a subclass of MG_FITS_Browser.
;
; :Private:
;-
pro comp_browser__define
  compile_opt strictarr

  define = { comp_browser, inherits mg_fits_browser, $
             draw_size: 0.0 $
           }
end


;= main routine

;+
; Create the browser.
;
; :Params:
;   pfilenames : in, optional, type=string
;     filenames of FITS files to view
;
; :Keywords:
;   filenames : in, optional, type=string
;     filenames of netCDF files to view
;   tlb : out, optional, type=long
;     set to a named variable to retrieve the top-level base widget identifier
;     of the FITS browser
;-
pro comp_browser, pfilenames, filenames=kfilenames, tlb=tlb, _extra=e
  compile_opt strictarr
  common comp_browser_common, browser

  ; parameter filename takes precedence (it clobbers keyword filename,
  ; if both present)

  if (n_elements(kfilenames) gt 0L) then _filenames = kfilenames
  if (n_elements(pfilenames) gt 0L) then _filenames = pfilenames

  if (obj_valid(browser)) then begin
    browser->load_files, _filenames
  endif else begin
    browser = mg_fits_browser(filenames=_filenames, $
                              tlb=tlb, $
                              classname='comp_browser', $
                              _extra=e)
  endelse
end

