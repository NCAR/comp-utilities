; docformat = 'rst'

function comp_compare_images, filename1, filename2, $
                              extension=extension, mask=mask
  compile_opt strictarr

  fits_open, filename1, fcb
  fits_read, fcb, data, header, /header_only
  fits_close, fcb

  date_obs = sxpar(header, 'DATE-OBS')
  date_tokens = strsplit(date_obs, '-', /extract)
  date = date_tokens[0] + date_tokens[1] + date_tokens[2]

  comp_make_mask, date, header, mask

  fits_open, filename1, fcb
  fits_read, fcb, data1, header1, exten_no=extension
  fits_close, fcb

  ind = where(data1 lt 0, count)
  if (count gt 0L) then message, string(count, filename1, format='(%"%d negative values in %s")'), /informational

  fits_open, filename2, fcb
  fits_read, fcb, data2, header2, exten_no=extension
  fits_close, fcb

  ind = where(data2 lt 0, count)
  if (count gt 0L) then message, string(count, filename2, format='(%"%d negative values in %s")'), /informational

  ratio = data1 / data2
  ind = where(mask eq 0.0, count)
  ratio[ind] = 0.0

  return, ratio
end


; main-level example

root = '/hao/compdata1/Data/CoMP'

subdir1 = 'process.backgrnd'
subdir2 = 'process.timetest'

fname = '20150624.170541.comp.1074.fts'

filename1 = filepath(fname, subdir=[subdir1, '20150624'], root=root)
filename2 = filepath(fname, subdir=[subdir2, '20150624'], root=root)

ratio = comp_compare_images(filename1, filename2, extension=1, mask=mask)

end
