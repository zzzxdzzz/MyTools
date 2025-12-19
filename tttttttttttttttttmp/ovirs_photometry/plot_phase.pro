;+
; NAME:
;
;  plot_phase.pro
;
; PURPOSE:
;
;  Plot disk-integrated phase function for input parameters
;
; CATEGORY:
;
;  Plotting
;
; CALLING SEQUENCE:
;
;  plot_phase[, model][, p][, infile=infile, bandno=bandno][, _extra = _extra]
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;   model   - String or array of string to specify model name:
;             lommel-seeliger, rolo, minnaert, mcewen
;   p       - Photometric parameters
;
; KEYWORD PARAMETERS:
;
;   infile  - String to specify photometric parameter FITS file
;   bandno  - Band number to be plotted
;   _extra  - Keywords accepted by IDL plot
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
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
; MODIFICATION HISTORY:
;
;  v1.0 : March 15, 2017, created by JYL @PSI
;  March 27, 2017, JYL @PSI
;    Use `message` to print information
;  April 7, 2017, JYL @PSI
;    Corrected a small bug
;-

PRO plot_phase, model, p, phase=pha, infile=infile, bandno=bandno, overplot=overplot, _extra=_extra

    IF (n_params() EQ 0) AND (NOT keyword_set(infile)) THEN BEGIN
        message,/info, 'Usage: plot_phase[, model][, p][, infile=infile, bandno=bandno][, _extra = _extra]'
        message,/info, '           model   - String or array of string to specify model name:'
        message,/info, '                     lommel-seeliger, rolo, minnaert, mcewen.'
        message,/info, '           p       - Photometric parameters'
        message,/info, '           infile  - String to specify photometric parameter FITS file'
        message,/info, '           bandno  - Band number to be plotted'
        message,/info, '           _extra  - Keywords accepted by IDL plot'
        message,/info
        message,/info, '* When `p` is not specified, `infile` and `bandno` have to be specified.'
        message,/info, '* If `model` is not specified when `infile` and `bandno` are'
        message,/info, '  present, then all models stored in `infile` can will be plotted.'
        message,/info, '* Program accepts keywords to IDL plot command'
        RETURN
    ENDIF

    IF NOT keyword_set(overplot) THEN overplot=0
    IF NOT keyword_set(pha) THEN pha=findgen(181.)
    IF n_params() LT 2 THEN BEGIN  ; need to specify input file
        IF NOT keyword_set(infile) THEN message, 'no parameters provided'
        IF NOT keyword_set(bandno) THEN message, 'no band number specified'
        IF n_params() EQ 1 THEN $
            IF n_elements(model) EQ 1 THEN mods=[model] ELSE mods=model  $
        ELSE mods = ['lommel-seeliger', 'mcewen']
        ls = [0,2,3,4]
        catch, err
        FOR i=0, n_elements(mods)-1 DO BEGIN
            p = (read_phomodel(infile, mods[i]))[bandno, *]
            IF err EQ 0 THEN BEGIN
                op = ((i NE 0) OR overplot) ? 1 : 0
                ;stop
                plot_phase, mods[i], p, phase=pha, overplot=op, linestyle=ls[i], _extra=_extra
                ;stop
            ENDIF ELSE BEGIN
                message, 'parameter not found in input file'
            ENDELSE
        ENDFOR
    ENDIF ELSE BEGIN
        phi = phase_func(model, p, pha)
        IF overplot THEN oplot, pha, phi, _extra=_extra  $
        ELSE plot, pha, phi, _extra=_extra
    ENDELSE

END
