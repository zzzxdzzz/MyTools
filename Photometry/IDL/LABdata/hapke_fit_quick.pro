pro hapke_fit_quick, csvfile
  compile_opt idl2

  ; --- Basic file check ---
  if file_test(csvfile, /regular) eq 0 then message, 'CSV not found: ' + csvfile

  ; --- Containers for numeric columns ---
  ibL = list() & ebL = list() & abL = list() & rbL = list()

  openr, lun, csvfile, /get_lun
  while ~eof(lun) do begin
    line = ''
    readf, lun, line
    line = strtrim(line, 2)
    if strlen(line) eq 0 then continue

    parts = strsplit(line, ',', /extract)
    if n_elements(parts) lt 4 then continue

    x0 = str2d_safe(parts[0])
    x1 = str2d_safe(parts[1])
    x2 = str2d_safe(parts[2])
    x3 = str2d_safe(parts[3])

    if (finite(x0) eq 0) or (finite(x1) eq 0) or (finite(x2) eq 0) or (finite(x3) eq 0) then continue

    ibL.add, x0 & ebL.add, x1 & abL.add, x2 & rbL.add, x3
  endwhile
  free_lun, lun

  n = ibL.count()
  if n le 0 then message, 'No numeric data rows parsed from CSV: ' + csvfile

  ib = dblarr(n) & eb = dblarr(n) & ab = dblarr(n) & rb = dblarr(n)
  for i=0, n-1 do begin
    ib[i] = ibL[i]
    eb[i] = ebL[i]
    ab[i] = abL[i]
    rb[i] = rbL[i]
  endfor

  ; --- Minimal physical cleanup ---
  ; Convert negative incidence to positive (temporary pragmatic fix)
  ib = abs(ib)

  ; Drop non-positive intensity points to avoid log/divide issues
  good = where(rb gt 0.d and finite(rb) and finite(ib) and finite(eb) and finite(ab), ng)
  if ng lt 3 then message, 'Too few valid points after filtering.'

  ib = ib[good] & eb = eb[good] & ab = ab[good] & rb = rb[good]

  print, 'N points (filtered): ', ng
  print, 'Inc range: ', min(ib), max(ib)
  print, 'Emi range: ', min(eb), max(eb)
  print, 'Phase range: ', min(ab), max(ab)
  print, 'Intensity range: ', min(rb), max(rb)

  limi = [[0.d, 1.d, 0.01d, -0.48d, 0.d], $
          [1.d, 1.d, 0.01d, -0.48d, 60.d]]

  ; --- Avoid halting on floating exceptions while diagnosing ---
  old = !except
  !except = 0

  parms = fitr_hapke(ib, eb, ab, rb, limits=limi)

  !except = old

  print, parms
end
