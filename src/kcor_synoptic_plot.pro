; docformat = 'rst'

pro kcor_synoptic_plot, dates, data, title=title
  compile_opt strictarr

  n_dates = n_elements(dates)

  start_date = dates[0]
  end_date   = dates[n_dates - 1L]
  n_days = mlso_dateobs2jd(end_date) - mlso_dateobs2jd(start_date) + 1
  n_days = ceil(n_days)

  map = fltarr(n_days, 720) + 0.0001
  for r = 0L, n_dates - 1L do begin
    date = dates[r]
    date_index = mlso_dateobs2jd(date) - mlso_dateobs2jd(start_date)
    date_index = ceil(date_index)
    map[date_index, *] = *data[r]
  endfor

  window, xsize=(30 * n_days + 50) < 1200, ysize=400, /free, title=title
  device, decomposed=0
  range = mg_range(map)
  if (range[0] lt 0.0) then begin
    minv = - (max(abs(range)))
    maxv = (max(abs(range)))

    r = [bytarr(128) + 255B, 2B * bindgen(128)]
    g = [bytarr(128), 2B * bindgen(128)]
    b = [bytarr(128), 2B * bindgen(128)]
    tvlct, r, g, b
    background = 128
  endif else begin
    minv = 0.0
    maxv = range[1]

    loadct, 0
    background = 0
  endelse
  mg_image, map, min_value=minv, max_value=maxv, $
            /axes, yticklen=-0.01, $
            background=background
end
