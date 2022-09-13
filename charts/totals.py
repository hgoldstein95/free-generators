import pandas as pd

totals = pd.read_csv("totals.dat")

# Finds the mean and standard deviation of totals, grouping by both experiment and algorithm
groups = totals.groupby(["experiment", "algorithm"])
mus = groups.mean()
sds = groups.std()

for experiment in totals.experiment.unique():
    for algorithm in totals.algorithm.unique():
        alg = "cgs" if algorithm == "Grad" else "rej"
        mu = mus.loc[experiment, algorithm].values[0].round(2)
        sd = sds.loc[experiment, algorithm].values[0].round(2)

        print(f"\\def\\{alg}{experiment}mu{{{mu}}}")
        print(f"\\def\\{alg}{experiment}sd{{{sd}}}")