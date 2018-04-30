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

  !null = label_date(date_format='%M %D')
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
            title='East limb', xtickformat='label_date', $
            position=[0.05, 0.6, 0.97, 0.95], /noerase, $
            yticks=2, ytickname=['S', 'E', 'N'], yminor=4
  mg_image, west_limb, jd_dates, min_value=minv, max_value=maxv, $
            /axes, yticklen=-0.005, xticklen=-0.01, $
            background=background, $
            title='West limb', xtickformat='label_date', $
            position=[0.05, 0.1, 0.97, 0.45], /noerase, $
            yticks=2, ytickname=['S', 'W', 'N'], yminor=4

  device, decomposed=odec
end
