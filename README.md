# GeoLift in Python Tutorial


## Overview

GeoLift is an end-to-end solution to measure Lift at a Geo-level using the latest developments in Synthetic Control Methods. Through this tutorial it is possible to run Geolift R functions as power calculations, inference and plots in Python. 

Check out the files below for a more detailed description of the main functionalities of the package:

- GeoLiftPythonWalkthrough.ipynb: the completed GeoLift walkthrough in python including GeoLift single and multicell.

- Demo.py: a simple GeoLift walkthrough in Python including only GeoLift single cell.


## Requirements 

GeoLift in Python version requires or works with:

- R version 4.0.0 or newer. Please check [R installaton](https://facebookincubator.github.io/GeoLift/docs/GettingStarted/InstallingR).

- Python version 3.7.0 or newer. Please check [Python installation](https://www.python.org/).


## Installation

The installation of GeoLift in Python version assumes that you will use pip3 as your Python package manager. If you do not use pip3, before running anything, edit the commands in setup.sh and change pip3 to your Python package manager to avoid package management issues.

Step 1: Donwload GeoLiftPython folder hosted on GeoLift's Github.

Step 2: In your terminal, go to the GeoLiftPython folder and run the following:
```
$ bash setup.sh
```
This command will:
1.1 install `virtualenv` python package and activate a virutal environment. 
1.2 install the `pandas`, `rpy2`, `ipython`, `jupyter` packages under `requirement.txt`.
1.3 install `augsynth` and `Geolift` R packages in `r_lang_requirements.txt`.
1.4 open Jupyter Notebook.


Step 3: Open the `GeoLiftPython.ipynb` directly from your Jupyter Notebook interface.

Happy GeoLifting!


## FAQ

FAQ 1:
- Have you closed your notebook and want to reopen it?

No need to reinstall everything.  All you need to do is reactivate your virtualenv with:

On macOS
```
$ source path/to/GeoLiftPython/venv_geolift_python/bin/activate
$ jupyter notebook
```
On Windows
```
$ source path/to/GeoLiftPython/venv_geolift_python/Scripts/activate
$ jupyter notebook
```
Check the [Virtualenv website](https://virtualenv.pypa.io/en/latest/user_guide.html#activators) for more information.

NOTE: if your GeoLift files are in a different folder than the one activated, you need to go to that folder before running `jupyter notebook`.

FAQ 2:
- Don't you want to install Jupyter?

You can remove jupyter from requirements.txt

FAQ 3:
- Would you like to run GeoLift outside of a virtualenv?

Run the commands below in your Python environment: 

```
$ pip3 install -r requirements.txt
$ python3 r_lang_requirements.py
```

## GeoLiftPython folder

In the GeoLiftPython folder you'll find:

- `GeoLiftPythonWalkthrough.ipynb`: the completed GeoLift walkthrough in python including GeoLift single and multicell.

- `Demo.py`: a simple GeoLift walkthrough in Python including only GeoLift single cell.

- `setup.sh`: all necessary commands to simplify the setup of Python's environment and packages installation to run GeoLift in Python. This will setup a virtual environmenta and install the packages specified in requirements.txt, in r_lang_requirements.py files and it will open the Jupyter notebook.

- `requirements.txt`: contains the rpy2, ipython, pandas and jupyter pacakges.

- `r_lang_requirements.py`: contains the commands to install augsynth and GeoLift R packages.

- `utils.py`: Wrapper functions for GeoLift. 

- `GeoLift_PreTest.csv`: simulated data of 40 US cities across 90 days included in the GeoLift package.

- `GeoLift_Test.csv`: simulated Single cell data of 40 US cities across 90 days of pre-test and 15 days of test included in the GeoLift package.

- `GeoLift_Test_MultiCell.csv`: simulated Multi-Cell data of 40 US cities across 90 days of pre-test and 15 days of test included in the GeoLift package. 

- `Plots folder`: where the GeoLift plots will be saved.

## Join the community 

[facebook page](https://www.facebook.com/groups/fbgeolift/)


## Contact

- [jussan@fb.com](jussan@fb.com), Jussan Da Silva Bahia Nascimento, Facebook Marketing Science Partner

- [aesquerra@fb.com](aesquerra@fb.com), Arturo Esquerra, Facebook Marketing Science Partner

- [nicocru@fb.com](nicocru@fb.com), Nicolas Cruces, Facebook Marketing Science Partner



## Licenses

GeoLift is MIT licensed, as found in the LICENSE file.

The rpy2, ipython and pandas pacakges are necessary. The jupyter package and virtualenv are recommend but not necessary.

- [Licence of rpy2](https://github.com/rpy2/rpy2/blob/master/LICENSE)

- [Licence of ipython](https://github.com/ipython/ipython/blob/main/LICENSE)

- [Licence of pandas](https://github.com/pandas-dev/pandas/blob/main/LICENSE)

- [Licence of virtualenv](https://github.com/pypa/virtualenv/blob/main/LICENSE)

- [License of jupyter](https://jupyter.org/about)
