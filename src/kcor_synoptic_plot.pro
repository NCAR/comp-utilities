; docformat = 'rst'

pro kcor_synoptic_plot, dates, data, level_names, title=title
  compile_opt strictarr

  level_factor = fltarr(n_elements(level_names)) + 1.0
  l15_indices = where(level_names eq 'L1.5', n_l15)
  if (n_l15 gt 0L) then level_factor[l15_indices] = 1.0e6

  n_dates = n_elements(dates)

  start_date = dates[0]
  end_date   = dates[n_dates - 1L]
  n_days = mlso_dateobs2jd(end_date) - mlso_dateobs2jd(start_date) + 1
  n_days = ceil(n_days)

  epsilon = 0.0001
  map = fltarr(n_days, 720) + epsilon
  means = fltarr(n_days) + !values.f_nan
  for r = 0L, n_dates - 1L do begin
    date = dates[r]
    date_index = mlso_dateobs2jd(date) - mlso_dateobs2jd(start_date)
    date_index = ceil(date_index)
    map[date_index, *] = *data[r] * level_factor[r]
    means[date_index] = mean(*data[r])
  endfor

  window, xsize=(30 * n_days + 50) < 1200, ysize=800, /free, title=title
  device, get_decomposed=odec
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

    loadct, 0, /silent
    background = 0
  endelse

  north_up_map = shift(map, 0, -180)
  east_limb = reverse(north_up_map[*, 0:359], 2)
  west_limb = north_up_map[*, 360:*]

  !null = label_date(date_format='%Z %M %D')
  jd_dates = dblarr(n_dates)
  for d = 0L, n_dates - 1L do begin
    year    = strmid(dates[d], 0, 4)
    month   = strmid(dates[d], 5, 2)
    day     = strmid(dates[d], 8, 2)

    hours   = strmid(dates[d], 11, 2)
    minutes = strmid(dates[d], 14, 2)
    seconds = strmid(dates[d], 17, 2)

    jd_dates[d] = julday(month, day, year, hours, minutes, seconds)
  endfor

  erase, background
  mg_image, east_limb, jd_dates, min_value=minv, max_value=maxv, $
            /axes, yticklen=-0.005, xticklen=-0.01, $
            background=background, $
            title=string(title, format='(%"%s (East limb)")'), $
            xtickformat='label_date', $
            position=[0.05, 0.55, 0.97, 0.95], /noerase, $
            yticks=4, ytickname=['S', 'SE', 'E', 'NE', 'N'], yminor=4
  mg_image, west_limb, jd_dates, min_value=minv, max_value=maxv, $
            /axes, yticklen=-0.005, xticklen=-0.01, $
            background=background, $
            title=string(title, format='(%"%s (West limb)")'), $
            xtickformat='label_date', $
            position=[0.05, 0.05, 0.97, 0.45], /noerase, $
            yticks=4, ytickname=['S', 'SW', 'W', 'NW', 'N'], yminor=4

  window, xsize=(30 * n_days + 50) < 1200, ysize=300, /free, title=title
  m = moment(means, /nan)
  device, decomposed=1
  plot, jd_dates, means, $
        xstyle=1, xtickformat='label_date'
  oplot, jd_dates, fltarr(n_elements(jd_dates)) + m[0]
  for s = 1, 2 do begin
    oplot, jd_dates, fltarr(n_elements(jd_dates)) + m[0] + s * sqrt(m[1]), linestyle=3 - s
    oplot, jd_dates, fltarr(n_elements(jd_dates)) + m[0] - s * sqrt(m[1]), linestyle=3 - s
  endfor

stop

  device, decomposed=odec
end
