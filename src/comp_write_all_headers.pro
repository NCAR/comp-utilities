; docformat = 'rst'

;+
; Write the primary header and all extension headers for a given CoMP file to a
; text file.
;
; :Params:
;   filename : in, required, type=string
;     FITS file (any type)
;   output_filename : in, required, type=string
;     filename to write text headers to
;-
pro comp_write_all_headers, filename, output_filename
  compile_opt strictarr

  openw, lun, output_filename, /get_lun
  printf, lun, file_basename(filename), $
          format='(%"***** %s, primary header *****")'

  fits_open, filename, fcb
  fits_read, fcb, data, header, exten_no=0, /header_only
  printf, lun, transpose(header)

  for e = 1L, fcb.nextend do begin
    fits_read, fcb, data, header, exten_no=e, /header_only
    printf, lun, e, format='(%"***** extension %d *****")'
    printf, lun, transpose(header)
  endfor

  fits_close, fcb

  free_lun, lun
end


; main-level example program

day = '20150925'

time = '071400'
comp_write_all_headers, day + '.' + time + '.FTS', $
                        day + '.' + time + '.headers.txt'

time = '093250'
comp_write_all_headers, day + '.' + time + '.FTS', $
                        day + '.' + time + '.headers.txt'

end
