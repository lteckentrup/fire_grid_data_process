### Import relevant sklearn libraries
### Preprocessing
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

### Methods
from sklearn.decomposition import PCA
from sklearn.model_selection import GridSearchCV
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.ensemble import GradientBoostingClassifier

### Interpretation
from sklearn.metrics import classification_report
from sklearn.metrics import ConfusionMatrixDisplay
from sklearn.metrics import confusion_matrix
from sklearn.metrics import roc_curve, auc

### Import tools for plotting
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
import matplotlib as mpl

### Other libraries
import pandas as pd
import random
import numpy as np

### Select seed following Jim
random.seed(1935)

### List of relevant fuel types ?? why are all grasslands excluded?
fueltypes = [3001, 3002, 3003, 3005, 3006, 3007, 3008, 3009, 3010, 3011,
             3012, 3013, 3014, 3015, 3021, 3022, 3023, 3024, 3025, 3026,
             3027, 3028, 3029, 3043, 3047, 3048, 3049, 3050, 3051]

### Read in features + target
df = pd.read_csv('ft.met.lai.csv')

### Drop irrelevant fuel types and nan
df = df[df['ft'].isin(fueltypes)].dropna()

### Generate fuel type groups
wet_forests = [3002,3006,3007,3011,3012,3013,3015]
dry_forests_woodlands = [3005,3008,3009,3022,3043]
high_flammability_shrublands = [3014,3021,3024,3029]
low_flammability_shrublands = [3001,3003,3010,3023]
noncombustible = [3047]
mallees = [3025,3026,3027,3028,3048,3049,3050,3051]

### Store broad fuel type groups in new dataframe
df_broad = df.copy()
df_broad['ft'] = df_broad['ft'].apply(lambda x: 1 if x in wet_forests else
                                                2 if x in dry_forests_woodlands else
                                                3 if x in high_flammability_shrublands else
                                                4 if x in low_flammability_shrublands else
                                                5 if x in noncombustible else
                                                6 if x in mallees else
                                                np.nan)

def prep_data(dataframe,reduce_dim):
    ### Select target
    y = dataframe['ft']

    ### Select predictors
    X = dataframe[['soil.density', 'clay', 'rad.short.jan', 'rad.short.jul',
                   'wi', 'curvature_profile', 'curvature_plan', 'tmax.mean',
                   'map', 'pr.seaonality', 'lai.opt.mean', 'soil.depth',
                   'uran_pot', 'thorium_pot', 'vpd.mean']]

    ### Reduce dimensions using PCA
    if reduce_dim == True:
        ### Standardize predictors
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(X)
        
        ### Perform PCA on standardized predictors
        pca = PCA()
        X_pca = pca.fit_transform(X_scaled)

        ### Calculate explained variance ratios for each principal component
        explained_variance_ratio = pca.explained_variance_ratio_

        ### Derive number of components: Keep at least 90% of the variance
        target_k = 0.9
        k = (explained_variance_ratio.cumsum() < target_k).sum() + 1

        ### Perform PCA to reduce dimensions
        pca = PCA(n_components=k)
        X_pca = pca.fit_transform(X_scaled)

        ### Split data in to training and test datasets
        X_train, X_test, y_train, y_test = train_test_split(X_pca, y, test_size=0.3)

        ### Get feature names
        feature_names = []
        ### Grab all feature names
        feature_names_full = X.columns

        ### Grab PCA components
        components = pca.components_

        ### Iterate over the first n rows of the components matrix
        for i in range(k):
            # Get i-th row of the components matrix
            row = components[i,:] 

            # Get index of the element with the highest absolute value
            max_index = np.argmax(np.abs(row))
            
            # Get the name of the corresponding feature
            feature_name = feature_names_full[max_index]

            # Print the feature name
            feature_names.append(feature_name)
    
    ### Use all predictors
    elif reduce_dim == False:
        ### Split data in to training and test datasets
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)      
        
        ### Get feature names
        feature_names = X.columns

    return(X_train, X_test, y_train, y_test, feature_names)

### Hypertuning using GridSearchCV
def hypertuning(classifier, dataframe, reduce_dim):
    ### Read in training data
    X_train, X_test, y_train, y_test, feature_names = prep_data(dataframe,True)

    ### Choose parameters for hypertuning
    classifiers = {
        'Naive Bayes': (GaussianNB(), 
                       {'var_smoothing': 
                        [1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1]
                        }
                       ),
        'Nearest Neighbor': (KNeighborsClassifier(), 
                             {'n_neighbors': [3, 5, 7, 9], 
                              'p': [1, 2, 3],
                              'weights': ['uniform', 'distance']}),
        'Random Forest': (RandomForestClassifier(), 
                          {'n_estimators': [10, 50, 100, 200], 
                          'max_depth': [None, 5, 10, 15],
                          'min_samples_split': [2, 5, 10, 20, 50, 100]
                          }
                         ),
        'Gradient Boost': (GradientBoostingClassifier(), 
                           {}
                          ),
        'Neural Network': (MLPClassifier(max_iter=100),
                           {'hidden_layer_sizes': [(10,30,10),(20,)],
                            'activation': ['tanh', 'relu'],
                            'solver': ['sgd', 'adam'],
                            'alpha': [0.0001, 0.05],
                            'learning_rate': ['constant','adaptive']
                            }
                           )
    }

    # Get the classifier and hyperparameter grid for the specified classifier
    clf, param_grid = classifiers[classifier]

    if param_grid:
        # Create the grid search object
        grid_search = GridSearchCV(clf, param_grid, cv=5, n_jobs=-1, 
                                   scoring='accuracy')

        # Fit the grid search object to the training data
        grid_search.fit(X_train, y_train)

        # Get the best hyperparameters
        best_params = grid_search.best_params_
        print(best_params)

    else:
        pass

def ML_function(reduce_dim,classifier,dataframe,n_est):
    ### Grab data
    X_train, X_test, y_train, y_test, feature_names = prep_data(dataframe,reduce_dim)

    ### Set up classifier and pass optimized parameters
    classifiers = {
        'Naive Bayes': GaussianNB(),
        'Nearest Neighbors': KNeighborsClassifier(weights='distance'),
        'Random Forest': RandomForestClassifier(n_jobs=-1),
        'Gradient Boost': GradientBoostingClassifier(),
        'Neural Network': MLPClassifier(hidden_layer_sizes=(10, 10))
        }
    
    clf = classifiers[classifier]

    ### Fit classifier on training data
    clf.fit(X_train, y_train)

    ### Predict on test data
    y_pred = clf.predict(X_test)

    ### Calculate overall accuracy
    accuracy = clf.score(X_test, y_test)
    print('Accuracy: ',accuracy)

    ### Calculate how many correct classifications
    correct = (y_test == y_pred).sum()
    print('Number of correct classifications: ', correct)

    ### Get importance: Only possible for random forest, neural network, 
    ### gradient boost
    importance_methods = {
        'Random Forest': lambda clf: clf.feature_importances_,
        'Gradient Boost': lambda clf: clf.feature_importances_,
        'Neural Network': lambda clf: np.sum(np.abs(clf.coefs_[0]), axis=1)
        }

    # Get method for obtaining feature importances for the specified classifier
    importance_method = importance_methods.get(classifier)

    if importance_method:
        # Get the feature importances using the specified method
        importances = importance_method(clf)
    
        # Generate dataframe for feature importance
        importances_df = pd.DataFrame({'feature': feature_names,
                                    'importance': importances})
        # print(importances_df.sort_values(by=['importance'], ascending=False))

    else:
        pass

    '''
    Alternative for importance: Shap
    import shap
    explainer = shap.Explainer(clf.predict, X_test)
    shap_values = explainer(X_test)
    shap.plots.beeswarm(shap_values)
    or
    shap.summary_plot(shap_values, plot_type='violin')
    '''

    if classifier == 'Random Forest':
        ### Calculate maximum depth of tree
        depths = [tree.tree_.max_depth for tree in clf.estimators_]
        max_depth = max(depths)

    ### Calculate area under ROC curve for fueltype 3001
    false_positive_rate, true_positive_rate, thresholds = roc_curve(y_test,
                                                                    y_pred,
                                                                    pos_label=1)
    roc_auc = auc(false_positive_rate, true_positive_rate)
    print('Area under ROC curve: ',roc_auc)

    # ### Grab classification report
    # print(classification_report(y_test, y_pred))

    return(y_test,y_pred)

def eval_rf(fuel_group,reduce_dim,n_est):
    if fuel_group == 'Broad':
        y_test,y_pred = ML_function(reduce_dim,df_broad,n_est) 
            
        ### Set up figure size
        fig, ax = plt.subplots(figsize=(6.4,4.8))

        ### Fontsize confusion matrix
        fontsize=10

        ### Title
        title='Confusion matrix (broad fuel type groups)'

        ### Figure name
        fname='confusion_matrix_broad_PCA.pdf'

    elif fuel_group == 'All':
        y_test,y_pred = rf_function(reduce_dim,df,n_est) 

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
        if txt.get_text() == '0.00':
            txt.set_text('0')

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

### Set number of trees
n_est = 100

### Create plots for confusion matrix for broad fuel type groups
eval_rf('Broad',True,n_est)
