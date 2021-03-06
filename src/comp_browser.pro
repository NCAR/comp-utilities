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
; Determine whether the data is raw, level 1, or level 2.
;
; :Returns:
;   0 (raw), 1 (level 1), 2 (level 2), or -1 (unknown)
;
; :Params:
;   data : in, required, type=fltarr
;     data read from FITS file
;   header : in, required, type=strarr
;     FITS header
;
; :Keywords:
;   filename : in, optional, type=string
;     filename corresponding to extension or file
;-
function comp_browser::get_level, data, header, filename=filename
  compile_opt strictarr

  ndims = size(data, /n_dimensions)
  if (ndims ne 2) then return, -1

  dims = size(data, /dimensions)
  if (array_equal(dims, [1024, 1024])) then begin
    basename = file_basename(filename)
    suffix_name = strmid(basename, 7, /reverse_offset)
    if (suffix_name eq 'flat.fts' || suffix_name eq 'dark.fts') then return, 1
    return, 0
  endif
  if (array_equal(dims, [620, 620])) then begin
    switch strtrim(sxpar(header, 'EXTNAME'), 2) of
      'Intensity':
      'Enhanced Intensity':
      'Corrected LOS velocity':
      'Uncorrected LOS velocity':
      'Line Width':
      'Integrated Stokes Q':
      'Integrated Stokes U':
      'Total Linear Polarization': 
      'I':
      'Q':
      'U': 
      'Linear Polarization':
      'Azimuth':
      'Radial Azimuth':
      'Doppler Velocity':
      'Uncorrected Doppler Velocity': begin
          return, 2
        end
      else: begin
          if (n_elements(filename) gt 0L) then begin
            re = '.*\.(mean|median|sigma).*\.fts'
            if (stregex(file_basename(filename), re, /boolean)) then return, 2
          endif
        end
    endswitch

    return, 1
  endif

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
    'BACKGROUND': bmp = self->_make_icon(bytarr(3) + 180B)
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

  titles = strarr(n_exts > 1)

  case type of
    'DARK': begin
        if (strmid(file_basename(filename), 7, /reverse_offset) eq 'dark.fts') then begin   ; level 1
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
        if (strmid(file_basename(filename), 7, /reverse_offset) eq 'flat.fts') then begin   ; level 1
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
        mask = ~finite(wavelength) or polarization_state eq ''
        if (n_zero_beam gt 0L) then begin   ; level 1
          for e = 1L, n_exts do begin
            titles[e - 1] = mask[e - 1] $
                              ? string(e, ext_names[e], $
                                       format='(%"ext %d: %s")') $
                              : string(e, $
                                       polarization_state[e - 1], $
                                       wavelength[e - 1], $
                                       format='(%"ext %d: %s @ %0.2f nm")')
          endfor
        endif else begin   ; raw
          beam_description = 'Corona in ' + (['UL', 'LR'])[beam_state > 0]

          for e = 1L, n_exts do begin
            titles[e - 1] = mask[e - 1] $
                              ? string(e, ext_names[e], $
                                       format='(%"ext %d: %s")') $
                              : string(e, $
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
;
; :Keywords:
;   filename : in, required, type=string
;     filename of file
;-
function comp_browser::extension_bitmap, ext_number, ext_name, ext_header, $
                                         filename=filename
  compile_opt strictarr

  type = comp_get_type(filename, ext_header)

  case type of
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
    'BACKGROUND': bmp = self->_make_icon(bytarr(3) + 180B)
    'DARK': begin
        if (ext_name eq 'Time' || ext_name eq 'Exposure') then begin
          bmp = 0
        endif else begin
          bmp = self->_make_icon(bytarr(3))
        endelse
      end
    'OPAL': begin
        if (ext_name eq 'Time' || ext_name eq 'Wavelength' || ext_name eq 'Exposure') then begin
          bmp = 0
        endif else begin
          bmp = self->_make_icon(bytarr(3) + 160B)
        endelse
      end
    'CALIBRATION':
    'DYNAMICS': bmp = self->_make_icon([255B, 200B, 200B])
    'POLARIZATION': bmp = self->_make_icon([255B, 220B, 220B])
    'MEAN': bmp = self->_make_icon([230B, 255B, 230B])
    'MEDIAN': bmp = self->_make_icon([230B, 255B, 230B])
    'SIGMA': bmp = self->_make_icon([230B, 255B, 230B])
    'QUICK_INVERT': bmp = self->_make_icon([230B, 255B, 230B])
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

  return, [['*.fts;*.fts.gz;*.FTS', '*.*'], $
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
;
; :Keywords:
;   filename : in, optional, type=string
;     filename of file containing image
;   dimensions : in, required, type=fltarr(2)
;     dimensions of target window
;-
pro comp_browser::display_image, data, header, filename=filename, dimensions=dimensions
  compile_opt strictarr

  level = self->get_level(data, header, filename=filename)
  if (level lt 0) then begin
    self->erase
    return
  endif

  dims = size(data, /dimensions)

  data_aspect_ratio = float(dims[1]) / float(dims[0])
  draw_aspect_ratio = float(dimensions[1]) / float(dimensions[0])

  if (data_aspect_ratio gt draw_aspect_ratio) then begin
    ; use y as limiting factor for new dimensions
    dims *= dimensions[1] / float(dims[1])
  endif else begin
    ; use x as limiting factor for new dimensions
    dims *= dimensions[0] / float(dims[0])
  endelse

  _data = frebin(data, dims[0], dims[1])

  top = 250

  ; display flats and darks as level 0 (since they are level 0, just collected
  ; into a single file)
  basename = file_basename(filename)
  suffix_name = strmid(basename, 7, /reverse_offset)
  if (suffix_name eq 'flat.fts' || suffix_name eq 'dark.fts') then level = 0

  case level of
    0: begin
        fits_open, filename, fcb
        fits_read, fcb, primary_data, primary_header, exten_no=0, /header_only
        fits_close, fcb

        normalize = sxpar(primary_header, 'NORMALIZ', count=normalize_present)
        datatype = sxpar(primary_header, 'DATATYPE', count=datatype_present)

        if (datatype_present && strtrim(datatype, 2) eq 'FLAT') then begin
          display_min = 0.0
          display_max = 84.0
        endif else begin
          display_min = 0.0
          display_max = 5000.0
        endelse
        power = 1.0
        loadct, 0, /silent
        image = bytscl((_data > 0.0)^power, min=display_min, max=display_max, top=top)
      end
    1: begin
        wave_type = self->get_wave_type(sxpar(header, 'WAVELENG'))
        loadct, 3, /silent
        pol_state = strtrim(sxpar(header, 'POLSTATE'), 2)
        case wave_type of
          '1074' : begin
              if (pol_state eq 'Q' || pol_state eq 'U') then begin
                image = bytscl(_data, min=-0.4, max=0.4, top=top)
              endif else if (pol_state eq 'V') then begin
                image = bytscl(_data, min=-0.1, max=0.1, top=top)
              endif else begin
                image = bytscl((_data > 0.0)^0.5, min=0.0, max=3.0, top=top)
              endelse            
            end
          '1079' : begin
              if (pol_state eq 'Q' || pol_state eq 'U') then begin
                image = bytscl(_data, min=-0.4, max=0.4, top=top)
              endif else if (pol_state eq 'V') then begin
                image = bytscl(_data, min=-0.1, max=0.1, top=top)
              endif else begin
                image = bytscl((_data > 0.0)^0.5, min=0.0, max=1.5, top=top)
              endelse
            end
          '1083' : begin
              ; TODO: this needs to be update for Q/U/V
              if (pol_state eq 'Q' || pol_state eq 'U' || pol_state eq 'V') then begin
                display_min = -4.0
                display_max =  4.0
              endif else begin
                display_min = 0.0
                display_max = 8.0
              endelse
              power = 0.3

              image = bytscl((_data > 0.0)^power, min=display_min, max=display_max, top=top)
            end
        endcase
      end
    2: begin
        extname = strtrim(sxpar(header, 'EXTNAME'), 2)
        switch extname of
          'Intensity': begin
              comp_aia_lct, wave=193, /load
              display_min = 1
              display_max = 5
              power = 0.5
              image = bytscl((_data > 0.0)^power, min=display_min, max=display_max, top=top)
              break
            end
          'Azimuth': begin
              loadct, 4, /silent
              tvlct, r, g, b, /get
              b[255] = 255
              tvlct, r, g, b
              image = bytscl(_data, min=0, max=180, top=254)
              break
            end
          'Radial Azimuth': begin
              ncolors = 255
              loadct, 6, /silent, ncolors=ncolors
              tvlct, r, g, b, /get
              r[0:ncolors - 1] = shift(r[0:ncolors - 1], ncolors / 2)
              g[0:ncolors - 1] = shift(g[0:ncolors - 1], ncolors / 2)
              b[0:ncolors - 1] = shift(b[0:ncolors - 1], ncolors / 2)
              tvlct, r, g, b
              ;tvlct, 128B, 128B, 128B, 255L   ; bad values are grey
              tvlct, 0B, 0B, 0B, 255L   ; bad values are black

              bad_ind = where(_data lt -90.0 or finite(_data) eq 0, n_bad_ind)
              image = bytscl(_data, min=-90.0, max=90.0, top=ncolors - 1)
              if (n_bad_ind gt 0L) then image[bad_ind] = 255B

              break
            end
          'Enhanced Intensity': begin
              comp_aia_lct, wave=193, /load
              image = _data
              break
            end
          'Doppler Velocity':
          'Uncorrected Doppler Velocity': begin
              restore, filepath('my_doppler_ct.sav', root=mg_src_root())
              tvlct, r, g, b

              display_min = -10
              display_max = 10
              image = bytscl(data, min=display_min, max=display_max, top=253)
              good_values = where(finite(data), $
                                  ncomplement=n_bad_values, complement=bad_values)
              if (n_bad_values gt 0) then image[bad_values] = 254

              image = congrid(image, dims[0], dims[1])
              break
            end
          'Uncorrected LOS velocity':
          'Corrected LOS velocity': begin
              restore, filepath('my_doppler_ct.sav', root=mg_src_root())
              tvlct, r, g, b

              display_min = -10
              display_max = 10
              image = bytscl(data, min=display_min, max=display_max, top=253)
              good_values = where(finite(data), $
                                  ncomplement=n_bad_values, complement=bad_values)
              if (n_bad_values gt 0) then image[bad_values] = 254

              fits_open, filename, fcb
              fits_read, fcb, intensity, intensity_header, extname='Intensity'
              fits_close, fcb

              thresh_unmasked = where(intensity le 1, n_thresh_unmasked)
              image[thresh_unmasked] = 254
              image = congrid(image, dims[0], dims[1])
              break
            end
          'Line Width': begin
              loadct, 4, /silent
              display_min = 25
              display_max = 55
              image = bytscl(_data, min=display_min, max=display_max, top=254)
              break
            end
          'Integrated Stokes Q': begin
              loadct, 0, /silent
              image = bytscl(_data)
              break
            end
          'Integrated Stokes U': begin
              loadct, 0, /silent
              image = bytscl(_data)
              break
            end
          'Linear Polarization':
          'Total Linear Polarization': begin
              loadct, 0, /silent
              display_min = -2.3
              display_max = -0.3
              image = bytscl(alog10(_data), min=display_min, max=display_max, /nan)
              break
            end
          else: begin
              wave_type = self->get_wave_type(sxpar(header, 'WAVELENG'))
              loadct, 3, /silent
              pol_state = strtrim(sxpar(header, 'POLSTATE', count=n_polstate), 2)
              if (n_polstate eq 0) then begin
                pol_state = extname
              endif

              sigma = stregex(file_basename(filename), '.*sigma.*', /boolean)
              if (sigma) then begin
                image = bytscl(_data, min=0, max=1.0, top=top)
              endif else begin
                case wave_type of
                  '1074' : begin
                      if (pol_state eq 'Q' || pol_state eq 'U') then begin
                        image = bytscl(_data, min=-0.4, max=0.4, top=top)
                      endif else if (pol_state eq 'V') then begin
                        image = bytscl(_data, min=-0.1, max=0.1, top=top)
                      endif else begin
                        image = bytscl((_data > 0.0)^0.5, min=0.0, max=5.0, top=top)
                      endelse
                    end
                  '1079' : begin
                      if (pol_state eq 'Q' || pol_state eq 'U') then begin
                        image = bytscl(_data, min=-0.4, max=0.4, top=top)
                      endif else if (pol_state eq 'V') then begin
                        image = bytscl(_data, min=-0.1, max=0.1, top=top)
                      endif else begin
                        image = bytscl((_data > 0.0)^0.5, min=0.0, max=3.5, top=top)
                      endelse
                    end
                  '1083' : begin
                      ; TODO: this need to be updated for Q/U/V
                      if (pol_state eq 'Q' || pol_state eq 'U' || pol_state eq 'V') then begin
                        display_min = 1.0
                        display_max = 4.0
                      endif else begin
                        display_min = 2.0
                        display_max = 10.0
                      endelse
                      power = 0.3
                      image = bytscl((_data > 0.0)^power, min=display_min, max=display_max, top=top)
                    end
                endcase
              endelse
            end
        endswitch
      end
    else: message, 'unknown level'
  endcase

  if (dims[0] gt dimensions[0] || dims[1] gt dimensions[1]) then begin
    xoffset = 0
    yoffset = 0
  endif else begin
    xoffset = (dimensions[0] - dims[0]) / 2
    yoffset = (dimensions[1] - dims[1]) / 2
  endelse

  device, decomposed=0

  tv, image, xoffset, yoffset
end


;+
; Overlay information on the image.
;
; :Params:
;   data : in, required, type=2D array
;     data to display
;   header : in, required, type=strarr
;     FITS header
;
; :Keywords:
;   filename : in, optional, type=string
;     filename of file containing image
;   dimensions : in, required, type=fltarr(2)
;     dimensions of target window
;-
pro comp_browser::annotate_image, data, header, filename=filename, dimensions=dimensions
  compile_opt strictarr

  ; back sure data is passed in
  if (n_elements(data) eq 0L $
        || size(data, /n_dimensions) ne 2L $
        || n_elements(header) eq 0L) then return

  dims = size(data, /dimensions)

  ; only annotating level 1 data (but including flats.fts)
  flat = strmid(file_basename(filename), 7, /reverse_offset) eq 'flat.fts'

  if (~array_equal(dims, [620, 620]) && ~flat) then return

  if (flat) then begin
    fieldstop_color = '00ff00'x
    occulter_color = '00ffff'x
  endif else begin
    fieldstop_color = 'ffffff'x
    occulter_color = '00ffff'x
  endelse
  t = findgen(361) * !dtor

  device, decomposed=1
  if (flat) then begin
    new_oxcnter1 = (sxpar(header, 'OXCNTRU1', count=new_flat) - 1.0) / dims[0]
    if (new_flat) then begin
      oxcnter1 = new_oxcnter1
      oycnter1 = (sxpar(header, 'OYCNTRU1') - 1.0) / dims[1]
      oradius1 = sxpar(header, 'ORADU1')
      oxcnter2 = (sxpar(header, 'OXCNTRU2') - 1.0) / dims[0]
      oycnter2 = (sxpar(header, 'OYCNTRU2') - 1.0) / dims[1]
      oradius2 = sxpar(header, 'ORADU2')

      fxcnter1 = (sxpar(header, 'FXCNTRU1') - 1.0) / dims[0]
      fycnter1 = (sxpar(header, 'FYCNTRU1') - 1.0) / dims[1]
      fradius1 = sxpar(header, 'FRADU1')
      fxcnter2 = (sxpar(header, 'FXCNTRU2') - 1.0) / dims[0]
      fycnter2 = (sxpar(header, 'FYCNTRU2') - 1.0) / dims[1]
      fradius2 = sxpar(header, 'FRADU2')
    endif else begin
      oycnter1 = (sxpar(header, 'OYCNTER1') - 1.0) / dims[1]
      oradius1 = sxpar(header, 'ORADIUS1')
      oxcnter2 = (sxpar(header, 'OXCNTER2') - 1.0) / dims[0]
      oycnter2 = (sxpar(header, 'OYCNTER2') - 1.0) / dims[1]
      oradius2 = sxpar(header, 'ORADIUS2')

      fxcnter1 = (sxpar(header, 'FXCNTER1') - 1.0) / dims[0]
      fycnter1 = (sxpar(header, 'FYCNTER1') - 1.0) / dims[1]
      fradius1 = sxpar(header, 'FRADIUS1')
      fxcnter2 = (sxpar(header, 'FXCNTER2') - 1.0) / dims[0]
      fycnter2 = (sxpar(header, 'FYCNTER2') - 1.0) / dims[1]
      fradius2 = sxpar(header, 'FRADIUS2')
    endelse

    ox1 = dimensions[0] * (oradius1 / dims[0] * cos(t) + oxcnter1)
    oy1 = dimensions[1] * (oradius1 / dims[1] * sin(t) + oycnter1)
    ox2 = dimensions[0] * (oradius2 / dims[0] * cos(t) + oxcnter2)
    oy2 = dimensions[1] * (oradius2 / dims[1] * sin(t) + oycnter2)

    plots, dimensions[0] * [oxcnter1], dimensions[1] * [oycnter1], $
           psym=1, /device, color=occulter_color
    plots, ox1, oy1, /device, color=occulter_color
    plots, dimensions[0] * [oxcnter2], dimensions[1] * [oycnter2], $
           psym=1, /device, color=occulter_color
    plots, ox2, oy2, /device, color=occulter_color

    fx1 = dimensions[0] * (fradius1 / dims[0] * cos(t) + fxcnter1)
    fy1 = dimensions[1] * (fradius1 / dims[1] * sin(t) + fycnter1)
    fx2 = dimensions[0] * (fradius2 / dims[0] * cos(t) + fxcnter2)
    fy2 = dimensions[1] * (fradius2 / dims[1] * sin(t) + fycnter2)

    plots, dimensions[0] * [fxcnter1], dimensions[1] * [fycnter1], $
           psym=1, /device, color=fieldstop_color
    plots, fx1, fy1, /device, color=fieldstop_color
    plots, dimensions[0] * [fxcnter2], dimensions[1] * [fycnter2], $
           psym=1, /device, color=fieldstop_color
    plots, fx2, fy2, /device, color=fieldstop_color
  endif else begin
    frpix1 = (sxpar(header, 'FRPIX1') - 1.0) / dims[0]
    frpix2 = (sxpar(header, 'FRPIX2') - 1.0) / dims[1]
    fradius = (sxpar(header, 'FRADIUS'))

    crpix1 = (sxpar(header, 'CRPIX1') - 1.0) / dims[0]
    crpix2 = (sxpar(header, 'CRPIX2') - 1.0) / dims[1]
    cradius = (sxpar(header, 'ORADIUS'))

    fx = dimensions[0] * (fradius / dims[0] * cos(t) + frpix1)
    fy = dimensions[1] * (fradius / dims[1] * sin(t) + frpix2)

    cx = dimensions[0] * (cradius / dims[0] * cos(t) + crpix1)
    cy = dimensions[1] * (cradius / dims[1] * sin(t) + crpix2)

    plots, dimensions[0] * [frpix1], dimensions[1] * [frpix2], $
           psym=1, /device, color=fieldstop_color
    plots, fx, fy, /device, color=fieldstop_color

    plots, dimensions[0] * [crpix1], dimensions[1] * [crpix2], $
           psym=1, /device, color=occulter_color
    plots, cx, cy, /device, color=occulter_color

    p_angle = sxpar(header, 'SOLAR_P0')
    post_angle = sxpar(header, 'POSTPANG')
    post_angle -= p_angle + 90.0
    post_angle *= !dtor

    pa_r = (cradius + fradius) / 2.0
    pa_x = dimensions[0] * (pa_r / dims[0] * cos(post_angle) + crpix1)
    pa_y = dimensions[1] * (pa_r / dims[1] * sin(post_angle) + crpix2)
    plots, [pa_x], [pa_y], /device, color=occulter_color, psym=2
  endelse
end


;+
; Determine if annotation is available for a given image.
;
; :Params:
;   data : in, required, type=2D array
;     data to display
;   header : in, required, type=strarr
;     FITS header
;
; :Keywords:
;   filename : in, optional, type=string
;     filename of file containing image
;-
function comp_browser::annotate_available, data, header, filename=filename
  compile_opt strictarr

  level = self->get_level(data, header, filename=filename)
  return, level ge 1L
end


;= event handling

;+
; Handle context menu events.
;
; Override this method if your subclass creates context menus in
; `create_draw_contextmenu`.
;
; :Params:
;   event : in, required, type=structure
;     `WIDGET_CONTEXT` event
;-
pro comp_browser::handle_contextmenu_events, event
  compile_opt strictarr

  if (n_elements(*self.current_data) eq 0L) then return

  uname = widget_info(event.id, /uname)
  self->datacoords_for_screen, self.contextmenu_loc[0], self.contextmenu_loc[1], $
                               x=x, y=y

  pol_state = strtrim(sxpar(*self.current_header, 'POLSTATE'), 2)
  beam = sxpar(*self.current_header, 'BEAM')
  comp_spectral_profile, self.current_filename, pol_state, beam, x, y, error=error
  if (error ne 0) then begin
    fmt = '(%"problem computing spectral profile for %s @ %s, beam: %d, x: %d, y: %d")'
    self->set_status, string(file_basename(self.current_filename), $
                             pol_state, beam, x, y, $
                             format=fmt)
  endif
end


;= widget lifecycle methods

function comp_browser::create_draw_contextmenu, draw_id
  compile_opt strictarr

  context_base = widget_base(draw_id, /context_menu)
  spectral_profile_button = widget_button(context_base, $
                                          value='Plot spectral profile', $
                                          uname='spectral_profile')
  return, context_base
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

