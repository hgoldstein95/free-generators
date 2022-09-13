import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

DV_COLOR = "#005AB5"
QC_COLOR = "#DC3220"

plt.style.use("bmh")

libertine = fm.FontProperties(fname="libertine.ttf", size=30)
libertine_med = fm.FontProperties(fname="libertine.ttf", size=25)
libertine_small = fm.FontProperties(fname="libertine.ttf", size=20)

d = pd.read_csv("diversity.csv")

fig, ax = plt.subplots(figsize=(20, 10))
ax.set_title("Diversity vs. Size", fontproperties=libertine)

for (i, experiment) in enumerate(d.experiment.unique()):
    for algorithm in d.algorithm.unique():
        data = d.loc[d.algorithm == algorithm].loc[d.experiment == experiment]

        alg = "CGS" if algorithm == "Grad" else "Rejection"
        ax.plot(data["size"],
                data["diversity"],
                color=DV_COLOR if algorithm == "Grad" else QC_COLOR,
                label=f"{experiment}, {alg}",
                linestyle=["dashed", "dotted", "dashdot", "solid"][i])

# fig.tight_layout()

ax.set_xlabel("Size", fontproperties=libertine_med)
ax.set_ylabel("Diversity", fontproperties=libertine_med)
ax.set_xticklabels(map(int, ax.get_xticks()), fontproperties=libertine_small)
ax.set_yticklabels(map(int, ax.get_yticks()), fontproperties=libertine_small)
ax.legend(prop=libertine_small, loc="upper right")
plt.savefig("diversity.png")
plt.clf()