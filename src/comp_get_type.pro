; docformat = 'rst'

;+
; Get the type of a file.
;
; :Returns:
;   string
;
; :Params:
;   filename : in, required, type=string
;     filename of file to check
;   header : in, required, type=strarr
;     header
;-
function comp_get_type, filename, header
  compile_opt strictarr

  basename = file_basename(filename)

  if (strmatch(basename, '*.polarization*.fts*')) then begin
    type = 'POLARIZATION'
  endif else if (strmatch(basename, '*.dynamics*.fts*')) then begin
    type = 'DYNAMICS'
  endif else if (strmatch(basename, '*.intensity.fts*')) then begin
    type = 'INTENSITY'
  endif else if (strmatch(basename, '*.mean.fts*')) then begin
    type = 'MEAN'
  endif else if (strmatch(basename, '*.median.fts*')) then begin
    type = 'MEDIAN'
  endif else if (strmatch(basename, '*.quick_invert.fts*')) then begin
    type = 'QUICK_INVERT'
  endif else if (strmatch(basename, '*.sigma.fts*')) then begin
    type = 'SIGMA'
  endif else begin
    cover = sxpar(header, 'COVER', count=count)
    if (count gt 0L) then begin
      if (cover eq 0) then begin
        type = 'DARK'
      endif else begin
        opal_value = sxpar(header, 'OPAL', count=count)
        if (count gt 0L) then begin
          if (opal_value eq 1) then begin
            cal_polarizer = sxpar(header, 'POLARIZR')
            type = cal_polarizer ? 'CALIBRATION' : 'OPAL'
          endif else begin
            if (strmatch(basename, '*bkg.fts*')) then begin
              type = 'BACKGROUND'
            endif else begin
              type = 'DATA'
            endelse
          endelse
        endif else begin
          type = 'UNKNOWN'
        endelse
      endelse
    endif else begin
      _type = strtrim(sxpar(header, 'DATATYPE', count=count), 2)
      if (count gt 0L && _type eq 'FLAT') then begin
        cal_polarizer = sxpar(header, 'POLARIZR')
        type = cal_polarizer ? 'CALIBRATION' : 'OPAL'
      endif else begin
        type = 'UNKNOWN'
      endelse
    endelse
  endelse

  return, type
end
