# GeoLift in Python Tutorial


## Overview

GeoLift is an end-to-end solution to measure Lift at a Geo-level using the latest developments in Synthetic Control Methods. Through this tutorial it is possible to run Geolift R functions as power calculations, inference and plots in Python. 

Check out the files below for a more detailed description of the main functionalities of the package:

- GeoLiftPythonWalkthrough.ipynb: the completed GeoLift walkthrough in python including GeoLift single and multicell.

- demo.py: a simple GeoLift walkthrough in Python including only GeoLift single cell.


## Requirements 

GeoLift in Python version requires or works with:

- R version 4.0.0 or newer. Please check [R installaton](https://facebookincubator.github.io/GeoLift/docs/GettingStarted/InstallingR).

- Python version 3.7.0 or newer. Please check [Python installation](https://www.python.org/).


## Installation

The installation of GeoLift in Python version assumes that you will use pip3 as your Python package manager. If you do not use pip3, before running anything, edit the commands in setup.sh and change pip3 to your Python package manager to avoid package management issues.

### Option 1 - Work environment

Step 1: Download GeoLiftPython folder hosted on GeoLift's Github.

Step 2 (On Terminal): Located in the GeoLiftPython folder, run the commands below in your environment: 

```
pip3 install -r requirements.txt
python3 r_lang_requirements.py
```

Step 2 (On Jupyter): Located in the GeoLiftPython folder, run the commands below in your environment: 

```
!pip3 install -r requirements.txt
%run r_lang_requirements.py
```

These commands will:
1.1 install the `pandas`, `rpy2`, `ipython`, `jupyter` packages under `requirement.txt`.
1.2 install `augsynth`, `CommutingZones` and `Geolift` R packages in `r_lang_requirements.txt`.

NOTE: 
If you don't want to install Jupyter, you can remove jupyter from requirements.txt

### Option 2 - Virtual environment (MacOS only)

Step 1: Download GeoLiftPython folder hosted on GeoLift's Github.

Step 2: In your terminal, go to the GeoLiftPython folder and run the following:
```
bash setup.sh
```
This command will:
1.1 install `virtualenv` python package and activate a virutal environment. 
1.2 install the `pandas`, `rpy2`, `ipython`, `jupyter` packages under `requirement.txt`.
1.3 install `augsynth`, `CommutingZones` and `Geolift` R packages in `r_lang_requirements.txt`.
1.4 open Jupyter Notebook.

Step 3: Open the `GeoLiftPython.ipynb` directly from your Jupyter Notebook interface.

NOTE: 
If you have closed your notebook, there is no need to reinstall everything.  All you need to do is reactivate your virtualenv with:

```
source path/to/GeoLiftPython/venv_geolift_python/bin/activate
jupyter notebook
```
If your GeoLift files are in a different folder than the one activated, you need to go to the folder where your GeoLift files are located before running `jupyter notebook`.


Happy GeoLifting!


## GeoLiftPython folder

In the GeoLiftPython folder you'll find:

- `GeoLiftPythonWalkthrough.ipynb`: the completed GeoLift walkthrough in python including GeoLift single and multicell.

- `demo.py`: a simple GeoLift walkthrough in Python including only GeoLift single cell.

- `setup.sh`: all necessary commands to simplify the setup of Python's environment and packages installation to run GeoLift in Python. This will setup a virtual environment and install the packages specified in requirements.txt, in r_lang_requirements.py files and it will open the Jupyter notebook.

- `requirements.txt`: contains the rpy2, ipython, pandas and jupyter pacakges.

- `r_lang_requirements.py`: contains the commands to install augsynth, CommutingZones and GeoLift R packages.

- `utils.py`: Wrapper functions for GeoLift. 

- `GeoLift_PreTest.csv`: simulated data of 40 US cities across 90 days included in the GeoLift package.

- `GeoLift_Test.csv`: simulated Single cell data of 40 US cities across 90 days of pre-test and 15 days of test included in the GeoLift package.

- `GeoLift_Test_MultiCell.csv`: simulated Multi-Cell data of 40 US cities across 90 days of pre-test and 15 days of test included in the GeoLift package. 

- `Plots folder`: where the GeoLift plots will be saved.

## Join the community 

[facebook page](https://www.facebook.com/groups/fbgeolift/)


## Contact

- [jussan@meta.com](jussan@meta.com), Jussan Da Silva Bahia Nascimento, Meta Marketing Science Partner

- [aesquerrac@gmail.com](aesquerrac@gmail.com), Arturo Esquerra

- [nicolascruces1994@gmail.com](nicolascruces1994@gmail.com), Nicolas Cruces



## Licenses

GeoLift is MIT licensed, as found in the LICENSE file.

The rpy2, ipython and pandas pacakges are necessary. The jupyter package and virtualenv are recommend but not necessary.

- [Licence of rpy2](https://github.com/rpy2/rpy2/blob/master/LICENSE)

- [Licence of ipython](https://github.com/ipython/ipython/blob/main/LICENSE)

- [Licence of pandas](https://github.com/pandas-dev/pandas/blob/main/LICENSE)

- [Licence of virtualenv](https://github.com/pypa/virtualenv/blob/main/LICENSE)

- [License of jupyter](https://jupyter.org/about)
