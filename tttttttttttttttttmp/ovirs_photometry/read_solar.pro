;+
; NAME:
;
;  load_solar.pro
;
; PURPOSE:
;
;  Load solar flux file
;
; CATEGORY:
;
;  I/O
;
; CALLING SEQUENCE:
;
;  load_solar, infile, wav, flux
;
; INPUTS:
;
;  infile     - A string to specify the input solar flux file
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;  normalized - If set, then the phase function is normalized to unity at zero
;               phase angle (phi(0) = 1).  Default is to return the phase
;               function such at phi(0) = geometric albedo
;
; OUTPUTS:
;
;  wav        - Returns an array of wavelength in nm
;  flux       - Returns an array of solar flux in W/[m2 nm]
;
; OPTIONAL OUTPUTS:
;
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
;  readcol.pro in Goddard IDL Astronomy User's Library
;    (http://idlastro.gsfc.nasa.gov)
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;  v1.0 : July 14, 2016, created by JYL @PSI
;  v1.0.1 : March 13, 2017, modified by JYL @PSI
;    Bug fix for wave length unit
;  April 7, 2017, JYL @PSI
;    Small improvement in `quiet` keyword
;-

PRO read_solar, infile, wav, flux, quiet=quiet

    IF NOT keyword_set(quiet) THEN quiet=!quiet
    IF ~quiet THEN print, 'Read solar flux from '+infile
    readcol, infile, wav, flux, format='f,f', silent=quiet, skipline=2

END
