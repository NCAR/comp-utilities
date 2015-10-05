; docformat = 'rst'

;+
; This handles data with NaNs inserted in the manner of
; `COMP_PLOT_CROSSTALKPARAMS`. It finds the "groups" of good data in
; `data`. It assumes the following about `data`:
;
;   1. there are not leading or trailing NaNs
;   2. there are not two or more consecutive NaNs
;
; :Returns:
;   `lonarr`; the output will give the index of the first element in
;   each group, including an extra element at the end of the array
;   with the value of 2 more than the index of the last element of the
;   last group
;
; :Params:
;   data : in, required, type=fltarr
;     data to find groups in
;
; :Keywords:
;   n_groups : out, optional, type=long
;     retrieve the number of groups in the data
;-
function comp_plot_crosstalkparams_groups, data, n_groups=n_groups
  compile_opt strictarr

  nan_ind = where(finite(data) eq 0, n_nan)
  n_groups = n_nan + 1L

  return, [0L, nan_ind + 1L, n_elements(data) + 1L]
end


function comp_plot_crosstalkparams_tojd, dt
  compile_opt strictarr

  t = strsplit(dt, '.', /extract)
  
  year   = long(strmid(t[0], 0, 4))
  month  = long(strmid(t[0], 4, 2))
  day    = long(strmid(t[0], 6, 2))

  hour   = long(strmid(t[1], 0, 2))
  minute = long(strmid(t[1], 2, 2))
  second = long(strmid(t[1], 4, 2))

  return, julday(month, day, year, hour, minute, second)
end


;+
; Plot the crosstalk paramaters.
;
; :Params:
;   log_path : in, required, type=string
;     path to logs directory, i.e.,
;     /hao/compdata1/Data/CoMP/logs.backgrnd
;   process_path : in, required, type=string
;     path to process directory, i.e., /hao/compdata1/Data/CoMP/process.backgrnd
;   date : in, required, type=string
;     date to check, i.e., '20150624'
;-
pro comp_plot_crosstalkparams, log_path, process_path, date, $
                               charsize=charsize, _extra=e
  compile_opt strictarr
  on_error, 2

  eng_path = filepath('', $
                      subdir=['engineering', strmid(date, 0, 4)], $
                      root=log_path)

  ; maximum gap (in minutes) before not connecting points
  min_gap = 30.0

  search_names = string(date, format='(%"%s.comp.*.crosstalk.txt")')
  search_path = filepath(search_names, root=eng_path)
  files = file_search(search_path, count=n_files)

  if (n_files eq 0L) then return

  n_lines = lonarr(n_files)
  wavelengths = strarr(n_files)

  for f = 0L, n_files - 1L do begin
    n_lines[f] = file_lines(files[f])
    wavelengths[f] = strmid(file_basename(files[f]), 14, 4)
  endfor

  total_n_lines = total(n_lines, /integer)

  times      = dblarr(total_n_lines)
  datetimes  = strarr(total_n_lines)
  coeffs     = dblarr(12, total_n_lines)
  gbu_scores = bytarr(total_n_lines)

  line = ''
  offset = 0L
  format = '(C(CYI04, CMOI02, CDI02, ".", CHI02, CMI02, CSI02))'

  for f = 0L, n_files - 1L do begin
    openr, lun, files[f], /get_lun

    for i = 0L, n_lines[f] - 1L do begin
      readf, lun, line
      tokens = strsplit(line, ',', /extract)

      times[i + offset] = comp_plot_crosstalkparams_tojd(tokens[0])
      datetimes[i + offset] = string(times[i + offset], format=format)
      coeffs[*, i + offset] = double(tokens[1:12])
    endfor

    free_lun, lun

    offset += n_lines[f]
  endfor

  ; get GBU scores

  gbu_n_lines = lonarr(n_files)
  for f = 0L, n_files - 1L do begin
    gbu_name = string(wavelengths[f], format='(%"GBU.%d.log")')
    gbu_filename = filepath(gbu_name, subdir=date, root=process_path)
    gbu_n_lines[f] = file_lines(gbu_filename) - 1L
  endfor

  gbu_total_n_lines = total(gbu_n_lines, /integer)
  all_gbu_scores = replicate({datetime: '', score: 0B}, gbu_total_n_lines)

  offset = 0L
  for w = 0L, n_files - 1L do begin
    gbu_name = string(wavelengths[w], format='(%"GBU.%d.log")')
    gbu_filename = filepath(gbu_name, subdir=date, root=process_path)

    openr, lun, gbu_filename, /get_lun
    readf, lun, line ; throw away header line

    for i = 0L, gbu_n_lines[w] - 1L do begin
      readf, lun, line
      tokens = strsplit(line, /extract)

      datetime_jd = comp_plot_crosstalkparams_tojd(tokens[0])
      datetime = string(datetime_jd - 10.0 /24.0, format=format)
      score = fix(tokens[5])

      all_gbu_scores[i + offset].datetime = datetime
      all_gbu_scores[i + offset].score = score
    endfor

    free_lun, lun

    offset += gbu_n_lines[w]
  endfor

  for i = 0L, total_n_lines - 1L do begin
    ind = where(datetimes[i] eq all_gbu_scores.datetime, count)
    if (count eq 0L) then begin
      message, 'date/time not found in GBU: ' + datetimes[i]
    endif
    gbu_scores[i] = all_gbu_scores[ind[0]].score
  endfor

  max_coeffs = max(coeffs, min=min_coeffs, dimension=2)

  time_range = [min(times, max=max_time), max_time]

  device, get_decomposed=odec
  device, decomposed=1

  !p.multi = [12, 3, 4, 0, 0]
  date_label = label_date(date_format=['%H:%I']) 
  titles = ['I', 'Q', 'U'] + 'V'
  ytitles = ['constant', 'x', 'y', 'xy']

  colors = mg_n_categories(n_files, brewer_ct=31)

  for c = 0L, 11L do begin
    end_line = -1L
    for f = 0L, n_files - 1L do begin
      begin_line = end_line + 1L
      end_line = begin_line + n_lines[f] - 1L

      mg_add_gaps, times[begin_line:end_line], coeffs[c, begin_line:end_line], $
                   min_gap_length=min_gap / (24.0 * 60.0), $
                   gap_value=!values.d_nan, $
                   x_out=t, y_out=y
      mg_add_gaps, times[begin_line:end_line], gbu_scores[begin_line:end_line], $
                   min_gap_length=min_gap / (24.0 * 60.0), $
                   gap_value=!values.d_nan, $
                   y_out=gbus

      bad_ind = where(gbus ne 0B, n_bad)
      if (n_bad gt 0L) then y[bad_ind] = !values.d_nan

      if (f eq 0) then begin
        plot, t, y, /nodata, $
              title=c lt 3 ? titles[c] : '', $
              ystyle=9, yrange=[min(min_coeffs[c]), max(max_coeffs[c])], $
              ytitle=c mod 3 eq 0 ? ytitles[c / 3] : '', $
              xrange=time_range, xstyle=9, $
              xtickformat='label_date', xtickunits='Time', $
              charsize=charsize, $
              _extra=e
        oplot, t, y, color=colors[f], _extra=e
      endif else begin
        oplot, t, y, color=colors[f], _extra=e
      endelse

      group_ind = comp_plot_crosstalkparams_groups(t, n_groups=n_groups)
      for g = 0L, n_groups - 1L do begin
        start_index = group_ind[g]
        end_index = group_ind[g + 1] - 2L
        rms = mg_rms(y[start_index:end_index], /nan)

        if (rms ne 0.0) then begin
          xloc = 0.5 * (t[start_index] + t[end_index])
          yloc = min(min_coeffs[c]) + (1.0 - 0.1 * f) * (max(max_coeffs[c]) - min(min_coeffs[c]))
          xyouts, xloc, yloc, $
                  string(rms, format='(E0.2)'), $
                  color=colors[f], $
                  charsize=charsize * 0.5, alignment=0.5, /data
        endif
      endfor

      ; give legend
      xyouts, t[-1], y[-1], wavelengths[f], color=colors[f], charsize=charsize * 0.5
    endfor
  endfor
  !p.multi = 0

  xyouts, 0.98, 0.98, date, /normal, charsize=charsize * 0.6, alignment=1.0
  device, decomposed=odec
end


; main-level example program

to_ps = 0B

if (keyword_set(to_ps)) then begin
  basename = 'crosstalk'
  mg_psbegin, /image, filename=basename + '.ps', $
              xsize=10, ysize=7.5, /inches
  charsize = 0.75
endif else begin
  window, xsize=1500, ysize=750, /free, title='Crosstalk params'
  charsize = 2.0
endelse

log_path = '/hao/compdata1/Data/CoMP/logs.backgrnd2'
process_path = '/hao/compdata1/Data/CoMP/process.backgrnd2'

;d = '20150608'
;d = '20150622'
date = '20150624'

comp_plot_crosstalkparams, log_path, process_path, date, charsize=charsize


if (keyword_set(to_ps)) then begin
  mg_psend
  mg_convert, basename, max_dimensions=[1000, 1000], output=im, $
              /to_png
  mg_image, im, /new_window
endif

end
