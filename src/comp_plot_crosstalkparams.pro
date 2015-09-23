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


pro comp_plot_crosstalkparams, path, date, charsize=charsize, _extra=e
  compile_opt strictarr

  ; maximum gap (in minutes) before not connecting points
  min_gap = 30.0

  search_names = string(date, format='(%"%s.comp.*.crosstalk.txt")')
  search_path = filepath(search_names, root=path)
  files = file_search(search_path, count=n_files)

  if (n_files eq 0L) then return

  n_lines = lonarr(n_files)
  wavelengths = strarr(n_files)

  for f = 0L, n_files - 1L do begin
    n_lines[f] = file_lines(files[f])
    wavelengths[f] = strmid(file_basename(files[f]), 14, 4)
  endfor

  total_n_lines = total(n_lines, /integer)

  times = dblarr(total_n_lines)
  coeffs = dblarr(12, total_n_lines)

  line = ''
  offset = 0L
  for f = 0L, n_files - 1L do begin
    openr, lun, files[f], /get_lun

    for i = 0L, n_lines[f] - 1L do begin
      readf, lun, line
      tokens = strsplit(line, ',', /extract)
      t = strsplit(tokens[0], '.', /extract)

      year   = long(strmid(t[0], 0, 4))
      month  = long(strmid(t[0], 4, 2))
      day    = long(strmid(t[0], 6, 2))

      hour   = long(strmid(t[1], 0, 2))
      minute = long(strmid(t[1], 2, 2))
      second = long(strmid(t[1], 4, 2))

      times[i + offset] = julday(month, day, year, hour, minute, second)

      coeffs[*, i + offset] = double(tokens[1:12])
    endfor

    free_lun, lun

    offset += n_lines[f]
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
        rms = sqrt(total((y[start_index:end_index])^2) / (end_index - start_index + 1L))

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

p = '/hao/compdata1/Data/CoMP/logs.joe3/engineering/2015/'

;d = '20150608'
;d = '20150622'
d = '20150624'

comp_plot_crosstalkparams, p, d, charsize=charsize


if (keyword_set(to_ps)) then begin
  mg_psend
  mg_convert, basename, max_dimensions=[1000, 1000], output=im, $
              /to_png
  mg_image, im, /new_window
endif

end
