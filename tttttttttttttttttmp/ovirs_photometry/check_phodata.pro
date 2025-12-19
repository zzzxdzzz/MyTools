;+
; NAME:
;
;  check_phodata.pro
;
; PURPOSE:
;
;  Check the validity of photometric data
;
; CATEGORY:
;
;  Photometric modeling
;
; CALLING SEQUENCE:
;
;  status = check_phodata(iof, pha, inc, emi[, ioferr=ioferr][, /quiet])
;
; INPUTS:
;
;  iof, pha, inc, emi - Photometric data array
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  ioferr     - Uncertainties associated with iof data
;  quiet      - If set, then program suppress all info message.  Default
;               is to be controlled by system variable !quiet.
;
; OUTPUTS:
;
;  Program returns a status code, and in some cases, the input data are
;  modified.
;
;  A status code of 0 means no problems
;  identified.  A positive integer indicates non-fatal problems found, and
;  the data are fixed.  A negative integer indicates a fatal problems found,
;  and the data shall not be used for photometric model fitting.
;
;    0  : No problem found
;    1  : Invalid angles (i<0 or i>90, e<0 or e>90, a<0 or a>180) found and
;         corresponding data removed.
;    2  : Unphysical geometric parameters (i, e, a) found and corresponding
;         data points removed.
;    4  : Unrealistic I/F data (equal or less than 0), and bad data points
;         removed
;    8  : Bad uncertainty data (equal or less than 0), and bad data points
;         removed.
;    16  : Too many bad uncertainty data are identified (number of good data
;         points < 20), therefore all uncertainties are removed (set to 1)
;    -1 : Case 1 but cannot be fixed because the number of good data points
;         is less than 20.
;    -2 : Case 2 but cannot be fixed because the number of good data points
;         is less than 20.
;    -4 : Case 3 but cannot be fixed because the number of good data points
;         is less than 20.
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
; PROCEDURE USED:
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : Jan 4, 2017, created by JYL @PSI
;  March 27, 2017, JYL @PSI
;    Use `message` and system variable !quiet to control info messages
;    Change keyword `verbose` to `quiet`
;  April 7, 2017, JYL @PSI
;    Use `print` rather than `message` to print out information messages
;-


FUNCTION check_phodata, iof, pha, inc, emi, ioferr=ioferr, quiet=quiet

    IF NOT keyword_set(ioferr) THEN BEGIN
        ioferr = iof
        ioferr[*] = 1.
    ENDIF
    IF NOT keyword_set(quiet) THEN quiet=!quiet

    status = 0
    good = where(ioferr GT 0, complement=bad)
    IF n_elements(good) LT 20 THEN BEGIN
        ioferr[*] = 1.
        IF ~quiet THEN print, 'Too many bad uncertainty values, uncertainty data ignored.'
        status = status OR 16
    ENDIF

    code = [1, 2, 4, 8]
    msgs = ['Invalid geometric parameters found', $
            'Unphysical geometric parameters found', $
            'Invalid I/F data points found', $
            'Invalid uncertainty data points found']

    FOR i=0, n_elements(code)-1 DO BEGIN

        CASE code[i] OF
            1 : logicexp = (pha GE 0) AND (pha LE 180.) AND (inc GE 0) AND (inc LE 90) AND (emi GE 0) AND (emi LE 90)
            2 : logicexp = (abs(inc-emi) LE pha) AND (inc+emi GE pha)
            4 : logicexp = iof GT 0
            8 : logicexp = ioferr GT 0
        ENDCASE
        good = where(logicexp, complement=bad)
        IF n_elements(good) LT 20 THEN BEGIN
            IF ~quiet THEN print, msgs[i]+', too few good data points remaining.'
            status = -code[i]
        ENDIF ELSE BEGIN
            iof = iof[good]
            pha = pha[good]
            inc = inc[good]
            emi = emi[good]
            ioferr = ioferr[good]
            IF ~quiet THEN print, msgs[i]+', '+string(n_elements(bad))+' data points removed.'
            status = status OR code[i]
        ENDELSE

        IF status LT 0 THEN RETURN, status

    ENDFOR

    RETURN, status

END
