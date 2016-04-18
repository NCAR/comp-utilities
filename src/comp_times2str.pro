; docformat = 'rst'

;+
; Create nicely formatted strings for a given decimal hour time.
;
; :Returns:
;   string/strarr
;
; :Params:
;   times : in, required, type=fltarr
;     decimal times in 0.0 to 24.0
;-
function comp_times2str, times
  compile_opt strictarr

  hours = floor(times)
  minutes = floor(60 * (times - hours))
  seconds = floor(60 * 60 * (times - hours - minutes / 60.0))

  hours = string(hours, format='(%"%02d")')
  minutes = string(minutes, format='(%"%02d")')
  seconds = string(seconds, format='(%"%02d")')

  return, hours + ':' + minutes + ':' + seconds
end
