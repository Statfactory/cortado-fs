import pandas as pd
from datetime import datetime
import cortado as cr
import numpy as np
from sklearn.metrics import roc_auc_score

csvpath = "./airlinetrain1m.csv"
df_pd = pd.read_csv(csvpath)
df_pd["Month"] = df_pd["Month"].astype("category")
df_pd["DayofMonth"] = df_pd["DayofMonth"].astype("category")
df_pd["DayOfWeek"] = df_pd["DayOfWeek"].astype("category")
df_pd["UniqueCarrier"] = df_pd["UniqueCarrier"].astype("category")
df_pd["Origin"] = df_pd["Origin"].astype("category")
df_pd["Dest"] = df_pd["Dest"].astype("category")
df_pd["dep_delayed_15min"] = df_pd["dep_delayed_15min"].astype("category")
eta = 0.1
nrounds = 100
max_depth = 6
df_cr = cr.DataFrame.from_pandas(df_pd)
deptime = cr.Factor.from_covariate(df_cr["DepTime"])
distance = cr.Factor.from_covariate(df_cr["Distance"])
deptime = deptime.cached()
distance = distance.cached()
dep_delayed_15min = df_cr["dep_delayed_15min"]
label = cr.Covariate.from_factor(dep_delayed_15min, lambda level: level == "Y")
factors = df_cr.factors +  [deptime,distance]
factors.remove(dep_delayed_15min)
start = datetime.now()
trees, pred_cr = cr.xgblogit(label, factors,  eta = eta, lambda_ = 1.0, gamma = 0.0, minh = 1.0, nrounds = nrounds, maxdepth = max_depth, slicelen=1000000)
end = datetime.now()
print("cortado elapsed: {e}".format(e=(end - start)))
y = label.to_array() # convert to numpy array
auc_cr = roc_auc_score(y, pred_cr) # cortado auc
print("cortado auc: {auc_cr}".format(auc_cr=auc_cr))
print(np.sum(pred_cr))
