; docformat = 'rst'

pro comp_plot_crosstalkparams, filename
  compile_opt strictarr

  ; maximum gap (in minutes) before not connecting points
  max_gap = 30.0

  n_lines = file_lines(filename)

  times = dblarr(n_lines)
  coeffs = dblarr(12, n_lines)

  line = ''
  openr, lun, filename, /get_lun
  for i = 0L, n_lines - 1L do begin
    readf, lun, line
    tokens = strsplit(line, ',', /extract)
    t = strsplit(tokens[0], '.', /extract)

    year   = long(strmid(t[0], 0, 4))
    month  = long(strmid(t[0], 4, 2))
    day    = long(strmid(t[0], 6, 2))

    hour   = long(strmid(t[1], 0, 2))
    minute = long(strmid(t[1], 2, 2))
    second = long(strmid(t[1], 4, 2))

    times[i] = julday(month, day, year, hour, minute, second)

    coeffs[*, i] = double(tokens[1:12])
  endfor
  free_lun, lun

  ind = sort(times)
  times = times[ind]
  coeffs = coeffs[*, ind]

  diffs = times[1:*] - times[0:-1]
  ind = where(diffs gt max_gap / (24.0 * 60.0), count)
  if (count gt 0L) then begin
    new_times = dblarr(n_lines + count)
    new_coeffs = dblarr(12, n_lines + count)
    ind = [-1, ind, n_lines - 1L]
    for c = 1L, count + 1L do begin
      new_times[ind[c - 1L] + c:ind[c] + c - 1L] = times[ind[c - 1L] + 1:ind[c]]
      if (c ne count + 1L) then new_times[ind[c] + c] = !values.d_nan

      new_coeffs[*, ind[c - 1L] + c:ind[c] + c - 1L] = coeffs[*, ind[c - 1L] + 1:ind[c]]
      if (c ne count + 1L) then new_coeffs[*, ind[c] + c] = !values.d_nan
    endfor

    times = new_times
    coeffs = new_coeffs
  endif

  max_coeffs = max(coeffs, min=min_coeffs, dimension=2)
  common_axes = 0B

  !p.multi = [12, 3, 4, 0, 1]
  date_label = label_date(date_format=['%H:%I']) 
  for c = 0L, 11L do begin
    ind = keyword_set(common_axes) ? 4L * lindgen(3) + c mod 4 : c
    plot, times, coeffs[c, *], $
          charsize=2.0, $
          ystyle=8, yrange=[min(min_coeffs[ind]), max(max_coeffs[ind])], $
          xstyle=9, xtickformat='label_date', xtickunits='Time'
  endfor
  !p.multi = 0
end


; main-level example program

window, xsize=1500, ysize=750, /free, title='Crosstalk params'
f = '/hao/compdata1/Data/CoMP/logs.joe2/engineering/2015/20150624.comp.crosstalk.txt'
comp_plot_crosstalkparams, f

end
