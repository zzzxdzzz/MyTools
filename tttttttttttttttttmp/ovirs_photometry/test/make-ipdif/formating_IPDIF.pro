; Purpose
;	Read IPDIF_ES_APPS.fits and reformat it into recent format
; History:
;	Created by XDZ @PSI  08/03/2017
PRO formating_IPDIF
	infile = '../IPDIF_ES_APPS_original.fits'
	outfile = '../IPDIF_ES_APPS.fits'

	iof  = mrdfits(infile, 0, header0, /silent)
	err  = mrdfits(infile, 1, header1, /silent)
	wav  = mrdfits(infile, 2, header2, /silent)
	info = mrdfits(infile, 3, header3, /silent)

	sclk = info.sclk
	lat  = info.lat
	lon  = info.lon
	inc  = info.incidence
	emi  = info.emission
	pha  = info.phase
	spatial_resolution = info.spatial_resolution
	range_to_sun = info.range_to_sun

	tags = ['sclk', 'bs_flag', 'fov_flag', 'bs_angle', 'lat', 'lon', 'range', 'bs_x', 'bs_y', 'bs_z', 'incidang', 'emissang', 'phaseang', 'fill_frac', 'smjax', 'smnax', 'sun_rng']
	newinfo = {}
	value = [sclk[0],0,0,0,lat[0],lon[0],0,0,0,0,inc[0],emi[0],pha[0],0,0,0,0]
	FOR i=0, n_elements(tags)-1 DO newinfo = create_struct(newinfo,tags[i],value[i])
	n = n_elements(inc)
	struct = replicate(newinfo, n)
	FOR i=0, n-1 DO BEGIN
		struct[i].sclk = sclk[i]
		struct[i].lat  = lat[i]
		struct[i].lon  = lon[i]
		struct[i].sun_rng  = range_to_sun[i]
		struct[i].incidang = inc[i]
		struct[i].emissang = emi[i]
		struct[i].phaseang = pha[i]
	ENDFOR

	mwrfits, iof, outfile, header0, create=overwrite, /silent
	mwrfits, err, outfile, header1, /silent
	mwrfits, wav, outfile, header2, /silent
	mwrfits, struct, outfile, header3, /silent
END
