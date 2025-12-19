To test this package, run test script from shell prompt:

./test_phot

Success:

Test script ends normally and print out a message:
"Test completed and successful"

Three cases of failure:

1. IDL quit with error messages printed out to stderr.  In this case, the test
script may quit abnormal.

2. Photometry programs quit with non-zero status code.  In this case, the
test script prints out a message:
"Test FAILED with code ##"

3. Photometry programs quit with zero status code, but the output files are
different from the comparison files.  In this case, the test script prints out
a message:
"All programs completed successfully, but at least one output file does not
match the comparison file"

#################################################

This directory contains the photometric modeling and correction software
package for OSIRIS-REx OVIRS data at asteroid Bennu.

This aareadme.txt file contains the minimal information to start to use the
software.  For more detailed information, read the software manual
(manual.txt) and the documents in each IDL program.

The top level programs are:
* photmods.pro: photometric model fitting
* photcorrs.pro: photometric correction
* boloalbedos.pro: bolometric Bond albedo calculation

To print the calling signature:

IDL> photmods, /help
IDL> photcorrs, /help
IDL> boloalbedos, /help

All programs have similar calling signature, and use their respective
configuration files to store all input parameters.  The default configuration
files are 'photmods.conf’, ‘photcorrs.conf' and ‘boloalbedos.conf’, all ASCII
files and can be edited by any text editor.

Taking photmods.pro as an example, to call the program with default
configuration (use test data):

IDL> photmods

Or users can specify input and output files in the command line:

IDL> photmods, 'SPDIF.fits', 'Bennu_phopar.fits'

Users can also specify the configuration file, models to be fitted, and
verbose mode in command line:

IDL> photmods, configuration='config_file_name', model=['model1', 'model2', ...]

Command line parameters always override the corresponding items in the
configuration file.

To display manual:

IDL> photmods, /manual

This software requires mpfit.pro (already included) and Goddard IDL Astronomy
User's Library (not included in the package).

###################################################

Other tools included in this package:
combine_fits.pro —- To combine IPDIF fits files.
plot_phase.pro   —- Plot disk-integrated phase function for input parameters.
plot_simple.pro  —- Plot fitting results with given models and wavelength.
fits_diff.pro  -- Compare two FITS files.
