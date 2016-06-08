; docformat = 'rst'

pro comp_spectral_profile, filename, pol_state, beam, x, y, error=error
  compile_opt strictarr

  error = 0L

  comp_query_file, filename, $
                   polarization_state=polarization_states, $
                   beam_state=beam_states, $
                   wavelength=wavelengths

  indices = where(polarization_states eq pol_state and beam_states eq beam, n_wavelengths)
  if (n_wavelengths eq 0L) then begin
    error = 1L
    return
  endif

  all_wavelengths = wavelengths[indices]
  all_spectrum = fltarr(n_wavelengths)
  fits_open, filename, fcb
  for w = 0L, n_wavelengths - 1L do begin
    fits_read, fcb, data, exten_no=indices[w] + 1
    all_spectrum[w] = data[x, y]
  endfor
  fits_close, fcb

  ; sort arrays to average duplicates in wavelengths
  sort_ind = sort(all_wavelengths)
  all_wavelengths = all_wavelengths[sort_ind]
  all_spectrum    = all_spectrum[sort_ind]

  uniq_ind = uniq(all_wavelengths)
  wavelengths = all_wavelengths[uniq_ind]
  spectrum = fltarr(n_elements(uniq_ind))
  last_ind = 0L
  for i = 0L, n_elements(uniq_ind) - 1L do begin
    spectrum[i] = mean(all_spectrum[last_ind:uniq_ind[i]])
    last_ind = uniq_ind[i] + 1
  endfor

  window, /free, xsize=600, ysize=250, $
          title=string(file_basename(filename), format='(%"Spectral plot for %s")')
          
  plot, wavelengths, spectrum, xstyle=9, ystyle=8, $
        title=string(file_basename(filename), pol_state, beam, x, y, $
                     format='(%"Spectrum for %s @ %s, beam %d, x:%d, y:%d")')
end


; main-level example program

filename = '20160605.184110.comp.1083.iquv.17.fts'
comp_spectral_profile, filename, 'I', 0, 154, 525

end
