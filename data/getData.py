import requests
import json
import pandas as pd
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from EQ_USGS import get_EQ_USGS_df

for start_num in range(1970, 2021, 1):
    
    # First quarter of the year
    
    start_str = f"""{start_num}-01-02"""
    end_str = f"""{start_num}-03-01"""
    
    Params = {}
    Params["get_format"] = "geojson" # Format for importing data

    Params["min_date"] = start_str # Minimum date for reporting the data
    Params["max_date"] = end_str # Maximum date for reporting the data

    Params["min_magnitude"] = "2" # Minimum magnitude of the reporting data
    Params["max_magnitude"] = "10" # Maximum magnitude of the reporting data

    Params["min_latitude"] = "24" # Minimum latitude
    Params["max_latitude"] = "50" # Maximum latitude

    Params["min_longitude"] = "-133" # Minimum longitude
    Params["max_longitude"] = "-107" # Maximum longitude

    Params["order_by"] = "time" # Ordering the data by parameters
    Params["limit_data"] = "20000" # Maximum number of data


    df = get_EQ_USGS_df(Params)
    
    file_path = "../datasets/imported_usgs_api/" + "eq_california_yr_" + str(start_num) + "_1_raw" + ".csv"
    df.to_csv(file_path)
    
    
    # Second quarter of the year
    
    start_str = f"""{start_num}-03-02"""
    end_str = f"""{start_num}-06-01"""
    

    Params["min_date"] = start_str # Minimum date for reporting the data
    Params["max_date"] = end_str # Maximum date for reporting the data

    df = get_EQ_USGS_df(Params)
    
    file_path = "../datasets/imported_usgs_api/" + "eq_california_yr_" + str(start_num) + "_2_raw" + ".csv"
    df.to_csv(file_path)
    
    # Third quarter of the year
    
    start_str = f"""{start_num}-06-02"""
    end_str = f"""{start_num}-09-01"""
    

    Params["min_date"] = start_str # Minimum date for reporting the data
    Params["max_date"] = end_str # Maximum date for reporting the data


    df = get_EQ_USGS_df(Params)
    
    file_path = "../datasets/imported_usgs_api/" + "eq_california_yr_" + str(start_num) + "_3_raw" + ".csv"
    df.to_csv(file_path)
    
    # Fourth quarter of the year
    
    start_str = f"""{start_num}-09-02"""
    end_str = f"""{start_num + 1}-01-01"""
    

    Params["min_date"] = start_str # Minimum date for reporting the data
    Params["max_date"] = end_str # Maximum date for reporting the data

    # Getting the dataframe calling the custom-built function
    df = get_EQ_USGS_df(Params)
    # Saving the dataframe as a .csv file
    file_path = "../datasets/imported_usgs_api/" + "eq_california_yr_" + str(start_num) + "_4_raw" + ".csv"
    
    df.to_csv(file_path)