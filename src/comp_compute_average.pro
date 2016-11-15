; docformat = 'rst'

;+
; Perform average on files, given method.
;
; :Params:
;   files : in, required, type=string/strarr
;     filenames to average; they must all be the same wave type
;   method : in, required, type=long
;     index of averaging method: 0 for mean, 1 for median
;
; :Keywords:
;   output_filename : in, required, type=string
;     output filename
;   error : out, optional, type=long
;     set to a named variable to retrieve error status: 0=no error, 1=differing
;     wave types
;-
pro comp_compute_average, files, method, output_filename=output_filename, $
                          error=error
  compile_opt strictarr

  error = 0L

  stokes = ['I', 'Q', 'U', 'V']
  n_stokes = n_elements(stokes)
  nx = 620
  ny = 620

  n_files = n_elements(files)
  date_dir = strmid(file_basename(files[0]), 0, 8)

  stokes_present = strarr(n_files)
  wave_type = ''
  for f = 0L, n_files - 1L do begin
    ; {date}.{time}.comp.{wave}.{stokes}.{npts}.fts.gz
    tokens = strsplit(file_basename(files[f]), '.', /extract)
    if (wave_type eq '') then begin
      wave_type = tokens[3]
    endif else begin
      if (tokens[3] ne wave_type) then begin
        error = 1L
        return
      endif
    endelse
    stokes_present[f] = strupcase(tokens[4])
  endfor

  ; loop over filenames and determine number of each images for each Stokes
  ; parameter
  numof_stokes = intarr(n_stokes)
  which_file = intarr(n_files, n_stokes)
  filenames = strarr(n_files)

  for f = 0L, n_files - 1L do begin
    filenames[f] = strmid(file_basename(files[f]), 0, 15)
    for s = 0L, n_stokes - 1L do begin
      if (strpos(stokes_present[f], stokes[s]) gt -1) then begin
        which_file[numof_stokes[s], s] = f
        ++numof_stokes[s]
      endif
    endfor
  endfor

  ; time period for this averaging
  year     = long(strmid(filenames[0], 0, 4))
  month    = long(strmid(filenames[0], 4, 2))
  day      = long(strmid(filenames[0], 6, 2))
  hour     = long(strmid(filenames[0], 9, 2))
  minute   = long(strmid(filenames[0], 11, 2))
  second   = long(strmid(filenames[0], 13, 2))
  start_jd = julday(month, day, year, hour, minute, second)

  year    = long(strmid(filenames[n_files - 1], 0, 4))
  month   = long(strmid(filenames[n_files - 1], 4, 2))
  day     = long(strmid(filenames[n_files - 1], 6, 2))
  hour    = long(strmid(filenames[n_files - 1], 9, 2))
  minute  = long(strmid(filenames[n_files - 1], 11, 2))
  second  = long(strmid(filenames[n_files - 1], 13, 2))
  end_jd = julday(month, day, year, hour, minute, second)

  duration = end_jd - start_jd
  mid_jd = start_jd + duration / 2
  caldat, mid_jd, utmonth, utday, utyear, uthour, utminute, utsecond

  utyear   = string(utyear, format='(I4)')
  utday    = string(utday, format='(I02)')
  utmonth  = string(utmonth, format='(I02)')
  uthour   = string(uthour, format='(I02)')
  utminute = string(utminute, format='(I02)')
  utsecond = string(round(utsecond), format='(I02)')

  ; construct FITS standard strings
  date_str = utyear + '-' + utmonth + '-' + utday
  time_str = uthour + ':' + utminute + ':' + utsecond
  
  fits_open, output_filename, output_fcb, /write

  ; use test file to get sample headers
  fits_open, files[0], test_fcb

  ; read the primary header to use for the output
  fits_read, test_fcb, d, primary_header, /header_only, exten_no=0
  sxdelpar, primary_header, 'DATE_HST'
  sxdelpar, primary_header, 'TIME_HST'
  sxaddpar, primary_header, 'LEVEL   ', 'L2'
  fits_close, test_fcb

  sxaddpar, primary_header, 'DATE-OBS', date_str, $
            ' [UTC] Averaging mid-point DATE: CCYY-MM-DD', after='TIMESYS'
  sxaddpar, primary_header, 'TIME-OBS', time_str, $
            ' [UTC] Averaging mid-point TIME: HH:MM:SS', after='DATE-OBS'
  sxaddpar, primary_header, 'DURATION', 24. * 60. * duration, $
            ' [minutes] Averaging duration', after='TIME-OBS', format='(f8.3)'

  sxdelpar, primary_header, 'OBS_PLAN'
  sxdelpar, primary_header, 'OBS_ID'

  ; use given 5-pt wavelengths
  case wave_type of
    '1074': waves = [1074.38, 1074.50, 1074.62, 1074.74, 1074.86]
    '1079': waves = [1079.54, 1079.66, 1079.78, 1079.90, 1080.02]
    else: begin
        fits_close, fcb
        return
      end
  endcase

  n_waves = n_elements(waves)
  sxaddpar, primary_header, 'NTUNES', n_waves

  ; TODO:
  ;comp_l2_update_version, primary_header

  fits_write, output_fcb, 0, primary_header

  ; compute averages
  back = fltarr(nx, ny, n_waves)

  ; summation of number of files going into average
  naverage = lonarr(n_stokes, n_waves)
  num_averaged = lonarr(n_stokes, n_waves)
  back_naverage = lonarr(n_waves)
  num_back_averaged = lonarr(n_waves)
  average_times = strarr(2, n_stokes, n_waves)

  for ist = 0L, n_stokes - 1L do begin
    if (numof_stokes[ist] eq 0) then continue
    for iw = 0L, n_waves - 1L do begin
      data = reform(fltarr(nx, nx, numof_stokes[ist], /nozero), $
                    nx, $
                    ny, $
                    numof_stokes[ist])

      header = !null
      for ifile = 0L, numof_stokes[ist] - 1L do begin
        filename = files[which_file[ifile, ist]]
        name = file_basename(filename)

        fits_open, filename, fcb
        fits_read, fcb, d, theader, /header_only, exten_no=0
        comp_make_mask, date_dir, theader, mask

        comp_inventory_l1, fcb, wave, pol

        good = where(pol eq stokes[ist] and wave eq waves[iw], count)
        if (count eq 0) then begin
          fits_close, fcb
          continue
        endif
        fits_read, fcb, dat, header, exten_no=good[0] + 1

        naverage[ist, iw] += sxpar(header, 'NAVERAGE')
        data[*, *, ifile] = dat * mask
        if (num_averaged[ist, iw] eq 0) then average_times[0, ist, iw] = strmid(name, 9, 6)
        num_averaged[ist, iw] += 1
        average_times[1, ist, iw] = strmid(name, 9, 6)

        fits_close, fcb
      endfor

      sxaddpar, header, 'LEVEL   ', 'L2'
      ename = stokes[ist] + ', ' + string(format='(f7.2)', waves[iw])

      ; calculate noise sigma

      ; format times
      hrs  = strmid(average_times[*, ist, iw], 0, 2)
      mins = strmid(average_times[*, ist, iw], 2, 2)
      secs = strmid(average_times[*, ist, iw], 4, 2)
      average_times[*, ist, iw] = hrs + ':' + mins + ':' + secs

      m = numof_stokes[ist]
      sm = sqrt(m)

      sxaddpar, header, 'POLSTATE', stokes[ist]
      sxaddpar, header, 'WAVELENG', waves[iw]
      sxaddpar, header, 'NAVERAGE', m
      sxaddpar, header, 'NFILES', num_averaged[ist, iw], ' Number of files used', $
                after='NAVERAGE'

      start_time = num_averaged[ist, iw] eq 0L ? 0L : average_times[0, ist, iw]
      end_time = num_averaged[ist, iw] eq 0L ? 0L : average_times[1, ist, iw]

      sxaddpar, header, 'AVESTART', start_time, $
                ' [UTC] Start of averaging HH:MM:SS', after='NAVERAGE'
      sxaddpar, header, 'AVEEND', end_time, $
                ' [UTC] End of averaging HH:MM:SS', after='AVESTART'

      ; find median and mean across image
      sxaddpar, header, 'NAVERAGE', naverage[ist, iw]

      ; write Stokes parameters to output files
      case method of
        0: begin
            aver = mean(data, dimension=3)
            sxaddpar, header, 'DATAMIN', min(aver), ' MINIMUM DATA VALUE'
            sxaddpar, header, 'DATAMAX', max(aver), ' MAXIMUM DATA VALUE'
            nans = where(finite(aver, /nan), count)
            fits_write, output_fcb, aver, header, extname=ename
          end
        1: begin
            med = median(data, dimension=3)
            sxaddpar, header, 'DATAMIN', min(med), ' MINIMUM DATA VALUE'
            sxaddpar, header, 'DATAMAX', max(med), ' MAXIMUM DATA VALUE'
            nans = where(finite(med, /nan), count)
            fits_write, output_fcb, med, header, extname=ename
          end
      endcase
    endfor
  endfor

  fits_close, output_fcb
end
