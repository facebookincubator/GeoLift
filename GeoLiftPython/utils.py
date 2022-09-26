#Import rpy2 packages
from rpy2.robjects.packages import importr, data

#Import rpy2 objects
from rpy2.robjects import pandas2ri, conversion, globalenv, default_converter

#To call R graphics
from rpy2.robjects.lib import grdevices

#To plot R graphics
from IPython.display import Image, display

#To perform local conversions between Python and R objects
from rpy2.robjects.conversion import localconverter

#Print function from R
rprint = globalenv.find("print")

#Import GeoLift
GeoLift = importr("GeoLift")

# COUNT is incremented each time GeoLiftPlot function is called and it's used to enumerate the charts' name.
COUNT = 0

#Load and return the dataset included in the GeoLift package
def GeoLiftData(string_data):
    """
    Receive as input the name of the dataset included in the GeoLift package. 
    Then load and return this dataset.
    The following datasets are available: GeoLift_Test.csv, GeoLift_PreTest.csv, GeoLift_Test_MultiCell.csv.
    Arguments:
        string_data: the dataset name as string
    Returns:
        Return the dataset included in the GeoLift package.
    """
    result = data(GeoLift).fetch(string_data)[string_data]
    return result
        
#Receive a specific GeoLift Plot function (defined by the `func` parameter), its arguments and display the plot. 
def GeoLiftPlot(func, topng = False, **kwargs):
    """
    Receive as input the name of the plot function included in the GeoLift package and its parameters.
    Then check if rprint() is needed depending on the plot function and display the plot.
    Finally, check if chart will be saved as a png file.
    The following plot functions are available: GeoPlot, plot_GeoLiftPower, plot_GeoLift, Lift_plot, absolute_value_plot, cumulative_value_plot, plot_MultiCellMarketSelection, plot_MultiCellPower, plot_GeoLiftMultiCell.
    For more details about a specific plot function and its parameters you can run `print(r.help("name of the function in R"))`. See the following command examples: `print(r.help("plot.GeoLiftPower"))`, `print(r.help("plot.GeoLiftMultiCell"))`. Note that to run the help command you need to replace "_" by "." in the function name, e.g. change plot_GeoLiftPower to plot.GeoLiftPower.
    Arguments:
        func: name of the plot function 
        topng: condition that evaluates if chart will be saved or not
        **kwargs: the parameters of the plot function
    Returns:
        Display and save the plot   
    """
    global COUNT
    
    if func == GeoLift.plot_GeoLiftPower or func == GeoLift.plot_GeoLift:
        with grdevices.render_to_bytesio(grdevices.png, width=1024, height=896, res=150) as img:
            rprint(func(**kwargs))
        display(Image(data=img.getvalue(), format='png', embed=True))
    else:
        with grdevices.render_to_bytesio(grdevices.png, width=1024, height=896, res=150) as img:
            func(**kwargs)
        display(Image(data=img.getvalue(), format='png', embed=True))
        
    if topng == True:
        COUNT = COUNT + 1
        file_name = str("Plots/") + func.__rname__ + str("_") + str("Plot_n") + str(COUNT) + str(".png")
        with open(file_name, "wb") as png:
            png.write(img.getvalue())
    
        print("The", func.__rname__, "chart was saved in", file_name)
        
#Convert R dataframe into Pandas if conv_type = "ToPandas" or convert Pandas dataframe into R if conv_type = "ToR" 
def ConvertDf(df, conv_type):
    """
    Receive as input a dataframe and the type of converison.
    Then Convert R dataframe into Pandas if conv_type = "ToPandas" or convert Pandas dataframe into R if conv_type = "ToR".
    Arguments:
        df: a dataframe
        conv_type: type of conversion
    Returns:
        Return the converted dataframe
    """
    if conv_type == "ToPandas":
        with localconverter(default_converter + pandas2ri.converter):
            result = conversion.rpy2py(df)
        return result
    elif conv_type == "ToR":
        with localconverter(default_converter + pandas2ri.converter):
            result = conversion.py2rpy(df)
        return result
    else:
        print("Choose conv_type = ToPandas or conv_type = ToR")


