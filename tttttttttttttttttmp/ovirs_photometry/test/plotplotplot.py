from jylipy.Photometry import *
from jylipy.OREx import *
from jylipy import *
from matplotlib.pyplot import *
from numpy import *
from astropy.io import fits

f=fits.open('/Users/ZOU/WORK/idl/git/sawg/ovirs_photometry/test/g_1900cm_20170503_sopie_spdif_v001_sis.fits')
f='/Users/ZOU/WORK/idl/git/sawg/ovirs_photometry/test/g_1900cm_20170503_sopie_corr_v001_sis.fits'
f='/Users/ZOU/WORK/OSIRIS-REx/201705-SOPIE1/localtest/iof_exp1p28_20170321_sopie1.fits'

f='/Users/ZOU/WORK/OSIRIS-REx/201703-SOPIE-practice/03212017testdata/SIS/exp_1p28/combined1p28all.fits'
d = fits.open(f)
pho = PhotometricData(iof=d[0].data[:,399],inc=d[3].data['INCIDANG'],emi=d[3].data['EMISSANG'],pha=d[3].data['PHASEANG'])
lat = d[3].data['LAT']
lon = d[3].data['LON']
figure(1)
clf()
pho.plot()
savefig(f.replace('.fits','_band400.jpg'))


# plot distribution of scattering angles
figure(2)
clf()
plot(pho.inc,lat,'.')
plot(pho.emi,lat,'.')
plot(pho.pha,lat,'.')	
pplot(xlabel='Angle (deg)',ylabel='Latitude (deg)',ylim=[-90,90],yticks=linspace(-90,90,7))
legend(['Incidence','Emission','Phase'],loc='center right')
savefig(f.replace('.fits','_geom.jpg'))


# plot lon-lat sample on the surface
clf()
plot(lon,lat,'.')
pplot(xlabel='Longitude (deg)',ylabel='Latitude (deg)',ylim=[-90,90],yticks=linspace(-90,90,7),xlim=[-180,180],xticks=linspace(-180,180,7))
savefig(f.replace('.fits','_lonlat.jpg'))


