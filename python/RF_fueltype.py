### Import relevant sklearn libraries
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import ConfusionMatrixDisplay
from sklearn.metrics import roc_curve, auc
### Import tools for plotting
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
import matplotlib as mpl
### Other libraries
import pandas as pd
import random
import numpy as np

### List of relevant fuel types ?? why are all grasslands excluded?
fueltypes = [3001, 3002, 3003, 3005, 3006, 3007, 3008, 3009, 3010, 3011,
            3012, 3013, 3014, 3015, 3021, 3022, 3023, 3024, 3025, 3026,
            3027, 3028, 3029, 3043, 3047, 3048, 3049, 3050, 3051]

### Read in features + target
df = pd.read_csv('ft.met.lai.csv')

### Drop irrelevant fuel types and nan
df = df[df['ft'].isin(fueltypes)].dropna()

### Generate dataframe with broad fuel groups
df_broad = df.replace([3002,3006,3007,3011,3012,3013,3015],1)
df_broad = df_broad.replace([3005,3008,3009,3022,3043],2)
df_broad = df_broad.replace([3014,3021,3024,3029],3)
df_broad = df_broad.replace([3001,3003,3010,3023],4)
df_broad = df_broad.replace([3047],5)
df_broad = df_broad.replace([3025,3026,3027,3028,3048,3049,3050,3051],6)

def rf_function(dataframe):
    ### Select features
    X = dataframe[['soil.density', 'clay', 'rad.short.jan', 'rad.short.jul', 
                   'wi', 'curvature_profile', 'curvature_plan', 'tmax.mean', 
                   'map', 'pr.seaonality', 'lai.opt.mean', 'soil.depth', 
                   'uran_pot', 'thorium_pot', 'vpd.mean']]     

    ### Select target      
    y = dataframe['ft']

    ### Select seed following Jim
    random.seed(1935)

    ### Split data in to training and test datasets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)

    ### Set up random forest classifier
    clf = RandomForestClassifier(n_estimators=10)

    ### Fit random forest on training data
    classifier = clf.fit(X_train, y_train)

    ### Predict on test data
    y_pred = clf.predict(X_test)

    ### Calculate overall accuracy
    accuracy = clf.score(X_test, y_test)
    print('Accuracy: '+str(accuracy))

    ### Calculate how many correct classifications
    correct = (y_test == y_pred).sum()    
    print('Number of correct classifications: '+str(correct))

    ### Grab feature importance and feature names
    importances = clf.feature_importances_
    feature_names = X.columns

    ### Generate dataframe for feature importance
    importances_df = pd.DataFrame({'feature': feature_names, 
                                   'importance': importances})

    print(importances_df.sort_values(by=['importance'],
          ascending=False))                               

    ### Calculate maximum depth of tree                               
    depths = [tree.tree_.max_depth for tree in clf.estimators_]
    max_depth = max(depths)

    ### Calculate area under ROC curve for fueltype 3001
    false_positive_rate, true_positive_rate, thresholds = roc_curve(y_test, 
                                                                    y_pred, 
                                                                    pos_label=3001)
    roc_auc = auc(false_positive_rate, true_positive_rate)
    print('Area under ROC curve: '+str(roc_auc))

    ### Grab classification report
    print(classification_report(y_test, y_pred))

    return(y_test,y_pred)

rf_function(df) 
