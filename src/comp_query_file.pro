; docformat = 'rst'

;+
; Procedure to inventory contents of comp data file, beam status, group
; association, wavelength, polarization state, data type and exposure time are
; returned.
;
; Groups are defind to be unique combinations of wavelength, beam and
; polarization state.
;
; Based on `COMP_INVENTORY`.
;
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
  cover = sxpar(header, 'COVER')
  if (cover eq 0) then begin
    type = 'DARK'
  endif else begin
    opal_value = sxpar(header, 'OPAL')
    if (opal_value eq 1) then begin
      type = 'OPAL'
    endif else begin
      type = 'DATA'
    endelse
  endelse
  cal_polarizer = sxpar(header, 'POLARIZR')

  cal_retarder = sxpar(header, 'RETARDER', count=count)
  if (count eq 0L) then cal_retarder = 0

  ; other keywords
  for i = 0L, n_extensions - 1L do begin
    fits_read, fcb, data, header, /header_only, exten_no=i + 1L
    beam_state[i] = sxpar(header, 'BEAM')
    wavelength[i] = sxpar(header, 'WAVELENG')
    polarization_state[i] = strcompress(sxpar(header, 'POLSTATE'), /remove_all)
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
