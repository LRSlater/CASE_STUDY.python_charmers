
import pandas as pd 
import numpy as np 
import matplotlib.pyplot as plt 

import sqlite3
salary_data = pd.read_csv('/Users/louisperruchon/documents/project_ds311/data/salary_data_states (2).csv', sep=";")
print(salary_data)