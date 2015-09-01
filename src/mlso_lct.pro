; docformat = 'rst'

;+
; Load colormap from disk file.
;
; :History:
;   19 April 1995, Andrew L. Stanger
;   1 September 2015 modified by mgalloy
;
; :Params:
;   lutname : in, required, type=string
;     filename of LUT table file
;-
pro mlso_lct, lutname
  compile_opt strictarr

  index = bytarr(256)
  red   = bytarr(256)
  green = bytarr(256)
  blue  = bytarr(256)

  c = 0B
  r = 0B
  g = 0B
  b = 0B

  ; read colormap from ASCII file with the format:
  ;   (4I4), index, red, green, blue

  openr, lun, lutname, /get_lun
  for i = 0L, 255L do begin
    readf, lun, c, r, g, b
    index[i] = c
    red[i] = r
    green[i] = g
    blue[i] = b
  endfor

  ; load RGB values to current color table
  tvlct, red, green, blue

  free_lun, lun
end
