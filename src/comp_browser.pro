; docformat = 'rst'

;+
; GUI browser for CoMP data.
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


;= CoMP specific overrides from MG_FITS_Browser

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

  return, ['*.fts', '*.FTS']
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

  ndims = size(data, /n_dimensions)
  if (ndims ne 2) then begin
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

  wave_type = self->get_wave_type(sxpar(header, 'WAVELENG'))
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

  top = 250
  image = bytscl(data^power, min=display_min, max=display_max, top=top)

  if (dims[0] gt geo_info.draw_xsize || dims[1] gt geo_info.draw_ysize) then begin
    xoffset = 0
    yoffset = 0
  endif else begin
    xoffset = (geo_info.draw_xsize - dims[0]) / 2
    yoffset = (geo_info.draw_ysize - dims[1]) / 2
  endelse

  old_win_id = !d.window
  wset, self.draw_id
  tvscl, image, xoffset, yoffset
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
