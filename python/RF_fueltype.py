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
### Wet forests
df_broad = df.replace([3002,3006,3007,3011,3012,3013,3015],1)
### Dry forests, woodlands
df_broad = df_broad.replace([3005,3008,3009,3022,3043],2)
### Shrublands with high flammability (?)
df_broad = df_broad.replace([3014,3021,3024,3029],3)
### Shrublands with low flammability (?)
df_broad = df_broad.replace([3001,3003,3010,3023],4)
### Not combustible
df_broad = df_broad.replace([3047],5)
### Mallees
df_broad = df_broad.replace([3025,3026,3027,3028,3048,3049,3050,3051],6)

def rf_function(dataframe):
    ### Select features
    X = dataframe[['soil.density', 'clay', 'rad.short.jan', 'rad.short.jul', 
                   'wi', 'curvature_profile', 'curvature_plan', 'tmax.mean', 
                   'map', 'pr.seaonality', 'lai.opt.mean', 'soil.depth', 
                   'uran_pot', 'thorium_pot', 'vpd.mean']]     

    ### Select target      
    y = dataframe['ft']

    ### Select seed (following Jim but maybe remove/ set to 42?
    ### Test whether results robust to seed at some point)
    random.seed(1935)

    ### Split data in to training and test datasets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)

    ### Set up random forest classifier
    clf = RandomForestClassifier(n_estimators=100)

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

def eval_rf(fuel_group):
    if fuel_group == 'Broad':
        y_test,y_pred = rf_function(df_broad) 
            
        ### Set up figure size
        fig, ax = plt.subplots(figsize=(6.4,4.8))

        ### Fontsize confusion matrix
        fontsize=10

        ### Title
        title='Confusion matrix (broad fuel type groups)'

        ### Figure name
        fname='confusion_matrix_broad.pdf'

    elif fuel_group == 'All':
        y_test,y_pred = rf_function(df) 

        ### Set up figure size
        fig, ax = plt.subplots(figsize=(12,9))

         ### Fontsize confusion matrix
        fontsize=6

        ### Title
        title='Confusion matrix (all fuel types)'

        ### Figure name
        fname='confusion_matrix_ind_fueltypes.pdf'

    ### Generate colormap for normalised values
    cmap = plt.cm.magma_r
    cmaplist = [cmap(i) for i in range(cmap.N)]
    cmap = mpl.colors.LinearSegmentedColormap.from_list('Custom cmap', 
                                                        cmaplist, cmap.N)
    bounds = np.arange(0,1.1,0.1)
    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)                                                    
    
    ### Plot confusion matrix
    disp = ConfusionMatrixDisplay.from_predictions(
        y_test,
        y_pred,
        cmap=cmap, 
        im_kw={'norm':norm},
        text_kw={'fontsize':fontsize},
        normalize='true',
        xticks_rotation='vertical',
        values_format='.2f',
        ax=ax
    )

    ### Plot exact zeros without decimal
    for iy, ix in np.ndindex(disp.text_.shape):
        txt = disp.text_[iy, ix]
        if txt.get_text() == "0.00":
            txt.set_text("0")

    ### Remove spines
    ax.spines['left'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)

    ### Set labels
    ax.set_xlabel('Prediction')
    ax.set_ylabel('Observed')

    ### Set title
    ax.set_title(title)

    ### Tight layout
    plt.tight_layout()

    ### Save figure
    plt.savefig(fname)

### Create plots for confusion matrix for broad fuel type groups
eval_rf('Broad')

### and individual fuel types
eval_rf('All')