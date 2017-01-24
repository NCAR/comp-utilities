; docformat = 'rst'

;+
; Returns an epoch time (seconds from Jan 1, 1970 UTC) for the creation time of
; the newest file in the directory hierarchy at `root`.
;
; :Returns:
;   long64
;
; :Params:
;   root : in, required, type=string
;     root directory to check
;-
function comp_newest_file, root
  compile_opt strictarr

  files = file_search(root, '*', count=n_files)
  ctime = 0LL
  for f = 0L, n_files - 1L do begin
    ctime >= (file_info(files[f])).ctime
  endfor

  return, ctime
end


; main-level example program

root = '/hao/mahidata1/Data/CoMP/process.centering15/20161112'
newest = comp_newest_file(root)

end
