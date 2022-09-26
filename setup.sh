echo Running GeoLift Python installation script!
ENV_NAME=venv_geolift_python

echo Installing virtualenv using pip3.
pip3 install virtualenv

echo Building and activating virtualenv.
virtualenv $ENV_NAME && . $ENV_NAME/bin/activate

echo Installing python pkg requirements in virtualenv.
pip3 install -r requirements.txt

echo Installing R pkg requirements in virtualenv.
python3 r_lang_requirements.py

echo Opening Jupyter notebook 
jupyter notebook