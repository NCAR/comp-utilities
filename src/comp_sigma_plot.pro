; docformat = 'rst'


;+
; :Params:
;   data : in, required, type="fltarr(n_pts, 2, n_cal_states, n_pol_states)"
;     data to plot, `data[*, 0, *, *]` is model data, `data[*, 1, *, *]` is actual
;     calibration data 
;-
pro comp_sigma_plot, data, $
                     polarization_states=pol_states, $
                     calibration_states=cal_states
  compile_opt strictarr

  dims = size(data, /dimensions)
  _pol_states = n_elements(pol_states) eq 0L $
                  ? ('I' + ['+Q', '-Q', '+U', '-U', '+V', '-V']) $
                  : pol_states
  angles = ['0', '45', '90', '135']
  _cal_states = n_elements(cal_states) eq 0L $
                  ? ['clear', 'ret in ' + angles, 'ret out ' + angles] $
                  : cal_states

  !p.multi = [0, 1, n_elements(_pol_states)]
  diff = reform(data[*, 0, *, *] - data[*, 1, *, *])
  pdiff = diff / reform(data[*, 1, *, *]) * 100.0
  for p = 0L, n_elements(_pol_states) - 1L do begin
    sigma = total((reform(data[*, 0, *, p] - data[*, 1, *, p]))^2, 2, /preserve_type)
    plot, [0, 1], /nodata, $
          psym=-1, $
          xstyle=9, xrange=[0, n_elements(_cal_states) - 1L], $
          ystyle=9, yrange=[min(pdiff, max=max_pdiff), max_pdiff], $
          title=string(_pol_states[p], strjoin(strtrim(sigma, 2), ', '), $
                       format='(%"%s (sigma: %s)")'), $
          xtitle='Calibration state', $
          xtickname=_cal_states, $
          xticks=8, $
          ytitle='100 * (model - data) / data'
    for xi = 0L, dims[0] - 1L do begin
      oplot, pdiff[xi, *, p], psym=-xi, linestyle=xi
    endfor
  endfor
  !p.multi = 0
end


; main-level example program

basename = 'cal-fixed-ret-ret2'
filename = filepath(basename + '.sav', $
                    root='/home/mgalloy/software/comp-pipeline.calibration')
restore, filename, /verbose

x = [712, 918, 912, 541, 447, 479]
y = [535, 414, 135, 63, 183, 423]

cals = ['1 0 0', $
        '1 1 -1', '1 1 44', '1 1 89', '1 1 134', $
        '1 0 -1', '1 0 44', '1 0 89', '1 0 134']
pols = 'I' + ['+Q', '-Q', '+U', '-U', '+V', '-V']

full_data = fltarr(n_elements(x), 2, n_elements(cals), n_elements(pols))

for xi = 0L, n_elements(x) - 1L do begin
  for p = 0L, n_elements(pols) - 1L do begin
    for c = 0L, n_elements(cals) - 1L do begin
      ind = where(datapols eq pols[p] and datacals eq cals[c], count)
      if (count ne 1) then begin
        message, string(pols[p], cals[c], count, $
                        format='(%"pol state: %s, cal state: %s found %d times")')
      endif
      full_data[xi, 0, c, p] = data[x[xi], y[xi], ind[0]]
      full_data[xi, 1, c, p] = cal_data[x[xi], y[xi], ind[0]]
    endfor
  endfor
endfor

mg_psbegin, filename=basename + '.ps', xsize=6, ysize=10, /inches, yoffset=0.5
comp_sigma_plot, full_data
mg_psend

end
