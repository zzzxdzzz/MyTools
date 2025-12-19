;+
; NAME:
;
;  read_spec.pro
;
; PURPOSE:
;
;  Read input spectral data file for photometric correction.
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  data = read_spec(infile[, error=error][, wav=wav][, info=info]
;                         [, header=header][, /quiet][...]
;
; INPUTS:
;
;  infile     - A string of the input PDIF file
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  quiet    - If set, then program suppress all infomation messages.
;             Default is to be controlled by system variable !quiet.
;  other keywords accepted by mrdfits.pro.
;
; OUTPUTS:
;
;  Program returns the photometric data
;
; OPTIONAL OUTPUTS:
;
;  error      - Returns the errors of photometric data
;  wav        - Returns the wavelength of photometric data
;  info       - Returns a structure array of the information table
;  header     - Returns a string array of the PDIF FITS header
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
;   mrdfits.pro and all subroutines used by mrdfits.pro in Goddard IDL
;   Astronomy User's Library (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : June 7, 2016, created by JYL @PSI
;  v1.1 : July 20, 2016, modified by JYL @PSI
;    Corrected a bug that cause the program not to load FITS header
;  v1.2 : Dec 16, 2016, modified by ZXD @PSI
;     * Changed info.incidence to info.incidang
;     * Changed info.emission to info.emissang
;     * Changed info.phase to info.phaseang
;     * Changed info.spatial_resolution /res to info.fill_fac /ffac
;  v1.3 : Mar 8, 2017, modified by JYL @PSI
;     * Propagate all fields in the input data table into `info`, rather
;       than just those relevant to this pipeline
;  March 27, 2017, JYL @PSI
;    Change keyword `verbose` to `quiet`
;    Add error handling
;  April 7, 2017, JYL @PSI
;    Use system variable !quiet to set default keyword `quiet`
;  May 1, 2017, modified by mikef@lpl
;    Adapt to ICD input data
;-

FUNCTION read_spec, infile, error=error, wav=wav, info=info, header=header, status=status, quiet=quiet, _extra=extra

    IF NOT keyword_set(quiet) THEN quiet=!quiet

    status = -3
    IF NOT file_test(infile) THEN BEGIN
        status=-1
        message, 'input file not found: '+infile
    ENDIF

    data = mrdfits(infile, 0, header, silent=quiet, status=s, _strict_extra=extra)
    IF s NE 0 THEN BEGIN
        status = s
        message, !error_state.msg
    ENDIF

    dsz = size(data)
    IF dsz[0] EQ 3 THEN BEGIN
        ; ICD formatted, 3 HDU (spectra:uncertainty:quality, wavelength, geometry)
        error = Reform(data[*, 1, *])
        data = Reform(data[*, 0, *])
        next_hdu = 1
    ENDIF ELSE IF (dsz[0] EQ 2) OR (dsz[0] EQ 1) THEN BEGIN
        ; SIS formatted, 4 HDUs (spectra, uncertainty, wavelength, geometry)
        error = mrdfits(infile, 1, silent=quiet, status=s, _strict_extra=extra)
        IF s NE 0 THEN BEGIN
            status = s
            message, !error_state.msg
        ENDIF
        next_hdu = 2
    ENDIF ELSE BEGIN
        status = -3
        message, 'Unrecognized I/F file.  Primary data unit has '+StrTrim(dsz[0], 2)+' dimensions.'
    ENDELSE

    wav = mrdfits(infile, next_hdu, silent=quiet, status=s, _strict_extra=extra)
    IF s NE 0 THEN BEGIN
        status = s
        message, !error_state.msg
    ENDIF
    next_hdu = next_hdu + 1

    tbl = mrdfits(infile, next_hdu, silent=quiet, status=s, _strict_extra=extra)
    IF s NE 0 THEN BEGIN
        status = s
        message, !error_state.msg
    ENDIF

    ; restructure info data
    tags = tag_names(tbl)
    info = {}
    FOR i=0, n_elements(tags)-1 DO info=create_struct(info, tags[i], tbl.(i))

    status = 0
    RETURN, data

END
