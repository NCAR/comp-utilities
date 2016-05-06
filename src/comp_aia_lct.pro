; docformat = 'rst'

;+
; Procedure to load 'standard' color tables for AIA images.
;
; :Uses:
;   nint
;
; :Params:
;   r : out, optional, type=bytarr(256)
;     byte array
;   g : out, optional, type=bytarr(256)
;     byte array
;   b : out, optional, type=bytarr(256)
;     byte array
;
; :Keywords:
;   wavelnth : in, required, type=float
;     wavelength in Angstrom
;   load : in, optional, type=boolean
;     if set, apply color table
;
; :Author:
;   Karel Schrijver, 2010/04/12
;
pro comp_aia_lct, r, g, b, wavelnth=wavelnth, load=load
  compile_opt strictarr
  on_error, 2

  ; load in standard color table from which to start
  if (not keyword_set(wavelnth)) then begin
    message, 'must set keyword WAVELNGTH'
  endif

  loadct, 3, /silent
  tvlct, r0, g0, b0, /get

  c0 = byte(findgen(256))
  c1 = byte(sqrt(findgen(256)) * sqrt(255.))
  c2 = byte(findgen(256)^2 / 255.)
  c3 = byte((c1 + c2 / 2.) * 255. / (max(c1) + max(c2) / 2.))

  ; allowed values of wavelnth:
  wave = [1600, 1700, 4500, 94, 131, 171, 193, 211, 304, 335]
  select = where(nint(wavelnth) eq wave)
  if (select[0] eq -1) then begin
    message, 'selected invalid wavelength/channel'
  endif

  case select of
    0: begin   ; 1600
        r = c3
        g = c3
        b = c2
      end
    1: begin   ; 1700
        r = c1
        g = c0
        b = c0
      end
    2: begin   ; 4500
        r = c0
        g = c0
        b = byte(b0 / 2)
      end
    3: begin   ; 94
        r = c2
        g = c3
        b = c0
      end
    4: begin   ; 131
        r = g0
        g = r0
        b = r0
      end
    5: begin   ; 171
        r = r0
        g = c0
        b = b0
      end
    6: begin   ; 193
        r = c1
        g = c0
        b = c2
      end
    7: begin   ; 211
        r = c1
        g = c0
        b = c3
      end
    8: begin   ; 304
        r = r0
        g = g0
        b = b0 ; changed from c2 on 2010/06/01, now to show standard IDL color table 3
      end
    9: begin   ; 355
        r = c2
        g = c0
        b = c1
      end
    endcase

  if (keyword_set(load)) then tvlct, r, g, b
end
