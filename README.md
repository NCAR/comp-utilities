# CoMP utilities

This library contains several routines for analyzing and visualizing various aspects of CoMP data.


## COMP_BROWSER

To examine a raw, L1, or L2 FITS data file, use `COMP_BROWSER`:

    IDL> comp_browser, 'raw.he/20150801/20150801.070011.FTS'

This presents a list of the extensions in the file. The data can be displayed using the standard visualization presents used in the pipeline. For example the raw data is displayed as:

![Raw data](src/raw-data.png "Raw data")

Level 1 data files can also be browsed in the same manner:

    IDL> comp_browser, 'process.l2test/20150801/20150801.165927.comp.1074.iqu.11.fts'

The Level 1 data is displayed as:

![L1 data](src/l1-data.png "L1 data")

The primary and extension headers can also be displayed for any FITS file. Below, the header is searched for text:

![L1 header](src/l1-header.png "L1 header")


## COMP_DIR_BROWSER

CoMP data, both raw and processed, is generally organized by date directories inside a top-level directory. `COMP_DIR_BROWSER` is used to quickly browse this directory and provide an inventory of the contents for each date. For example, to load the *process.l2test* directory containing only one day of Level 2 data, use:

    IDL> comp_dir_browser, 'process.l2test'

Similarly, *raw.he*, containing a months worth of data, is opened in the same manner:

    IDL> comp_dir_browser, 'raw.he'

Selecting a date in the *raw.he* list shows the raw FITS data files (typically only darks, flats, and science images) in chronological order for the day:

![Raw directory](src/raw-dir.png "Raw directory")

A processed date, such as one from *process.l2test*, can contain many types of FITS output files:

![L2 directory](src/l2-dir.png "L2 directory")

Right clicking on a file or selection of files brings up a context menu to display those files in a `COMP_BROWSER` window for further examination.

It is useful to use the `CALIBRATION` keyword when displaying raw calibration to show a few extra calibration related fields:

    IDL> comp_dir_browser, 'raw.calibration', /calibration

![Raw calibration directory](src/raw-cal-dir.png "Raw calibration directory")


## COMP_LOG_BROWSER

The pipeline output logs and observer logs can be displayed with `COMP_LOG_BROWSER`:

    IDL> comp_log_browser, 'logs.l2test', observer_log_dir='/hao/ftp/d5/mlso/log/observer'

![Log browser](src/log-browser.png "Log browser")

The log browser can filter the output log messages by severity. This allows jobs to be run with a verbose logging setting (DEBUG), but to display only a more restrictive set of messages chosen later.

The log browser updates its content automatically, so it can be used to monitor currently running jobs.


## COMP_RUN_BROWSER

The inputs and outputs of a CoMP pipeline run is determined by a configuration file. To display the input and output files using `COMP_DIR_BROWSER` and the output log files using `COMP_LOG_BROWSER`, pass the configuration file used for the run to `COMP_RUN_BROWSER`:

    IDL> comp_run_browser, 'config/comp.mgalloy.compdata.l2test.cfg'
