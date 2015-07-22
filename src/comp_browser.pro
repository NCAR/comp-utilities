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
;   ext_number : in, required, type=long
;     extension number
;   ext_name : in, required, type=long
;     extension name
;   ext_header : in, required, type=strarr
;     header for extension
;-
function comp_browser::extension_title, ext_number, ext_name, ext_header
  compile_opt strictarr

  datatype   = sxpar(ext_header, 'DATATYPE', count=count)
  if (count eq 0L) then return, '--'

  polstate   = strtrim(sxpar(ext_header, 'POLSTATE', count=polstate_count), 2)
  if (polstate_count eq 0L) then begin
    if (ext_name ne '') then begin
      tokens = strsplit(ext_name, ',', /extract)
      polstate = strtrim(tokens[0], 2)
    endif else begin
      polstate = '--'
    endelse
  endif

  wavelength = sxpar(ext_header, 'WAVELENG')
  beam       = sxpar(ext_header, 'BEAM', count=beam_count)
  if (beam_count gt 0L) then begin
    beam_desc  = beam gt 0 ? '(FG in LR)' : '(FG in UL)'
  endif else beam_desc = ''

  return, string(datatype, polstate, wavelength, beam_desc, $
                 format='(%"%s: %s @ %0.2f %s")')
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

  data = congrid(data, dims[0], dims[1])

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

  image = bytscl((data > 0.0)^power, min=display_min, max=display_max, top=top)

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


;= lifecycle methods

;+
; Define CoMP_Browser class, a subclass of MG_FITS_Browser.
;
; :Private:
;-
pro comp_browser__define
  compile_opt strictarr

  define = { comp_browser, inherits mg_fits_browser }
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
pro comp_browser, pfilenames, filenames=kfilenames, tlb=tlb
  compile_opt strictarr
  
  ; parameter filename takes precedence (it clobbers keyword filename,
  ; if both present)

  if (n_elements(kfilenames) gt 0L) then _filenames = kfilenames
  if (n_elements(pfilenames) gt 0L) then _filenames = pfilenames

  b =  mg_fits_browser(pfilenames, filenames=kfilenames, tlb=tlb, $
                       classname='comp_browser')
end

