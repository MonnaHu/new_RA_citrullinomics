
conda activate pytorch11
import pandas as pd
import shap
from sklearn import neighbors

# input data
data = pd.read_csv("H:/20250330 figure+code/Figure 5/Predicting MTX+LEF response & Fig. 5b-h/training.csv")
data.head(10)
data.columns
x = data.iloc[:,2:]
x.columns
y = data.iloc[:, 1]

# Train the KNN model
n_neighbors = 5
knn = neighbors.KNeighborsClassifier(n_neighbors,weights='distance')
knn.fit(x, y)
# Produce the SHAP values
knn_explainer = shap.KernelExplainer(knn.predict,x)
knn_shap_values = knn_explainer.shap_values(x)

shap.summary_plot(knn_shap_values, x)



######################################################## MTXHCQ
data = pd.read_csv("H:/20250330 figure+code/Figure 5/Predicting MTX+HCQ response & Fig. 5i-o/training.csv")
data.head(10)
data.columns
x = data.iloc[:,2:]
x.columns
y = data.iloc[:, 1]

# Train the KNN model
n_neighbors = 9
knn = neighbors.KNeighborsClassifier(n_neighbors,weights='distance')
knn.fit(x, y)
# Produce the SHAP values
knn_explainer = shap.KernelExplainer(knn.predict,x)
knn_shap_values = knn_explainer.shap_values(x)

shap.summary_plot(knn_shap_values, x)显示每个特征如何影响该样本的预测。例如，如果某个特征的SHAP值为正，则表示该特征增加了该样本被预测为特定类别的概率；反之为负则表示减少了。