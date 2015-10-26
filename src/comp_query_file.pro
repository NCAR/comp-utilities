; docformat = 'rst'

;+
; Procedure to inventory contents of CoMP raw data file: beam status, group
; association, wavelength, polarization state, data type and exposure time are
; returned.
;
; Groups are defind to be unique combinations of wavelength, beam and
; polarization state.
;
; Based on `COMP_INVENTORY`.
;
; :Params:
;   filename : in, required, type=string
;     CoMP FITS file to query
;
; :Keywords:
;   group : out, optional, type=intarr
;     groups by extension
;   beam_state : out, optional, type=intarr
;     position of foreground/background beam
;   polarization_state : out, optional, type=strarr
;     polarization state, 'I+V', etc.
;   type : out, optional, type=string
;     type of CoMP data file "DARK", "OPAL", or "DATA"
;   exposure : out, optional, type=float
;     exposure time in milliseconds
;   cover : out, optional, type=int
;     cover present
;   cal_polarizer : out, optional, type=int
;     polarizer present
;   cal_retarder : out, optional, type=int
;     retarder present
;   observation_id : out, optional, type=string
;     ID for observation within an `OBSERVATION_PLAN`
;   observation_plan : out, optional, type=string
;     plan for file, i.e., synoptic, waves, etc.
;-
pro comp_query_file, filename, $
                     group=group, $
                     beam_state=beam_state, $
                     wavelength=wavelength, $
                     polarization_state=polarization_state, $
                     type=type, $
                     exposure=exposure, $
                     cover=cover, $
                     cal_polarizer=cal_polarizer, $
                     cal_retarder=cal_retarder, $
                     observation_id=observation_id, $
                     observation_plan=observation_plan
  compile_opt idl2

  fits_open, filename, fcb
  n_extensions = fcb.nextend   ; number of images in file

  beam_state = intarr(n_extensions)
  wavelength = fltarr(n_extensions)
  polarization_state = strarr(n_extensions)

  type = ''
  fits_read, fcb, data, header, /header_only, exten_no=0

  observation_id = strtrim(sxpar(header, 'OBS_ID', count=count), 2)
  if (count eq 0L) then observation_id = ''
  observation_plan = strtrim(sxpar(header, 'OBS_PLAN', count=count), 2)
  if (count eq 0L) then observation_plan = ''

  ; type
  cover = sxpar(header, 'COVER', count=count)
  if (count gt 0L) then begin
    if (cover eq 0) then begin
      type = 'DARK'
    endif else begin
      opal_value = sxpar(header, 'OPAL', count=count)
      if (count gt 0L) then begin
        if (opal_value eq 1) then begin
          type = 'OPAL'
        endif else begin
          type = 'DATA'
        endelse
      endif else begin
        type = 'UNKNOWN'
      endelse
    endelse
  endif else begin
    _type = strtrim(sxpar(header, 'DATATYPE', count=count), 2)
    if (count gt 0L && _type eq 'FLAT') then begin
      type = 'OPAL'
    endif else begin
      type = 'UNKNOWN'
    endelse
  endelse

  cal_polarizer = sxpar(header, 'POLARIZR')

  cal_retarder = sxpar(header, 'RETARDER', count=count)
  if (count eq 0L) then cal_retarder = 0

  ; other keywords
  for i = 0L, n_extensions - 1L do begin
    fits_read, fcb, data, header, /header_only, exten_no=i + 1L
    beam_state[i] = sxpar(header, 'BEAM', count=count)
    if (count eq 0L) then beam_state[i] = !values.f_nan
    wavelength[i] = sxpar(header, 'WAVELENG', count=count)
    if (count eq 0L) then wavelength[i] = !values.f_nan
    polarization_state[i] = strcompress(sxpar(header, 'POLSTATE', count=count), /remove_all)
    if (count eq 0L) then polarization_state[i] = ''
    exposure = sxpar(header, 'EXPOSURE')
  endfor

  ; group observations with like wavelength, polarization state, type and beam
  if (arg_present(group)) then begin
    group = intarr(n_extensions)
    group[0] = 0
    n_groups = 1

    for i = 1L, n_extensions - 1L do begin
      for j = 0L, i - 1L do begin
        if (wavelength[i] eq wavelength[j] $
              and polarization_state[i] eq polarization_state[j] $
              and beam_state[i] eq beam_state[j]) then begin
          group[i] = group[j]
          goto, done
        endif
      endfor

      group[i] = n_groups
      ++n_groups
      done:
    endfor
  endif

  fits_close, fcb
end
