import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import numpy as np
from scipy import interpolate

DV_COLOR = "#005AB5"
QC_COLOR = "#DC3220"

plt.style.use("bmh")
plt.rcParams["figure.figsize"] = (10, 6)  # type: ignore

libertine = fm.FontProperties(fname="libertine.ttf", size=30)
libertine_med = fm.FontProperties(fname="libertine.ttf", size=25)
libertine_small = fm.FontProperties(fname="libertine.ttf", size=20)


def plot_sizes(experiment):

    def normalize(xs):
        return np.array(list(map(lambda x: x / sum(xs), xs)))

    width = 0.45

    dv_sizes = normalize(
        [int(line.rstrip()) for line in open("Grad_" + experiment + "_sizes.dat", "r").readlines()])
    dv_range = np.arange(len(dv_sizes)) + 1 + width / 2

    qc_sizes = normalize(
        [int(line.rstrip()) for line in open("QCRS_" + experiment + "_sizes.dat", "r").readlines()])
    qc_range = np.arange(len(qc_sizes)) + 1 - width / 2

    fig, ax = plt.subplots()
    ax.bar(
        qc_range,
        qc_sizes,
        width,
        color=QC_COLOR,
        label="Rejection",
    )
    ax.bar(
        dv_range,
        dv_sizes,
        width,
        color=DV_COLOR,
        label="CGS",
    )

    qc_range_smooth = np.linspace(qc_range.min(), qc_range.max(), 200)
    qc_sizes_smooth = interpolate.make_interp_spline(qc_range, qc_sizes,
                                                     bc_type="clamped")(qc_range_smooth)
    ax.plot(
        qc_range_smooth,
        qc_sizes_smooth,
        color=QC_COLOR,
    )
    ax.fill_between(qc_range_smooth, qc_sizes_smooth, color=QC_COLOR, alpha=0.2)

    dv_range_smooth = np.linspace(dv_range.min(), dv_range.max(), 200)
    dv_sizes_smooth = interpolate.make_interp_spline(dv_range, dv_sizes,
                                                     bc_type="clamped")(dv_range_smooth)
    ax.plot(
        dv_range_smooth,
        dv_sizes_smooth,
        color=DV_COLOR,
    )
    ax.fill_between(dv_range_smooth, dv_sizes_smooth, color=DV_COLOR, alpha=0.2)

    ax.set_title(experiment + ": Normalized Size Distribution", fontproperties=libertine)
    ax.set_xlabel("Term Size", fontproperties=libertine_med)
    ax.set_ylabel("Proportion of Results", fontproperties=libertine_med)
    ax.set_xticks(np.arange(len(dv_sizes)) + 1)
    ax.set_xticklabels(np.arange(len(dv_sizes)) + 1, fontproperties=libertine_small)
    ax.set_yticklabels(
        map(lambda x: "%.2f" % x, ax.get_yticks()),
        fontproperties=libertine_small,
    )
    ax.legend(prop=libertine_small)

    fig.tight_layout()

    plt.savefig(experiment + "_sizes.png")
    plt.clf()


def plot_times(experiment):
    dv_data = [
        int(line.rstrip()) for line in open("Grad_" + experiment + "_stamps.dat", "r").readlines()
    ]
    qc_data = [
        int(line.rstrip()) for line in open("QCRS_" + experiment + "_stamps.dat", "r").readlines()
    ]

    fig, ax = plt.subplots()

    ax.set_title(experiment + ": Unique Terms over Time", fontproperties=libertine)
    ax.plot(qc_data, [i for i in range(len(qc_data))], color=QC_COLOR, label="Rejection")
    ax.plot(dv_data, [i for i in range(len(dv_data))], color=DV_COLOR, label="CGS")
    ax.set_xlabel("Time (sec)", fontproperties=libertine_med)
    ax.set_ylabel("Unique Terms", fontproperties=libertine_med)
    ax.set_xticklabels(map(lambda x: int(x / 1000), ax.get_xticks()),
                       fontproperties=libertine_small)
    ax.set_yticklabels(map(int, ax.get_yticks()), fontproperties=libertine_small)
    ax.legend(prop=libertine_small)

    fig.tight_layout()

    plt.savefig(experiment + "_times.png")
    plt.clf()


def plot_freqs(experiment):
    dv_freqs = [(line.rstrip().split(",")[1], int(line.rstrip().split(",")[2]))
                for line in open("Grad_" + experiment + "_freqs.dat", "r").readlines()]
    qc_freqs = [(line.rstrip().split(",")[1], int(line.rstrip().split(",")[2]))
                for line in open("QCRS_" + experiment + "_freqs.dat", "r").readlines()]

    dv_freqs = [(name, count / sum(map(lambda x: x[1], dv_freqs))) for (name, count) in dv_freqs]
    qc_freqs = [(name, count / sum(map(lambda x: x[1], qc_freqs))) for (name, count) in qc_freqs]

    width = 0.45

    fig, ax = plt.subplots()

    ax.set_title(experiment + ": Normalized Constructor Frequency", fontproperties=libertine)
    ax.bar(
        np.arange(len(dv_freqs)) + width / 2,
        [x[1] for x in dv_freqs],
        width,
        color=DV_COLOR,
        label="CGS",
    )
    ax.bar(
        np.arange(len(qc_freqs)) - width / 2,
        [x[1] for x in qc_freqs],
        width,
        color=QC_COLOR,
        label="Rejection",
    )
    ax.set_xlabel("Node Type", fontproperties=libertine_med)
    ax.set_ylabel("Norm. Frequency", fontproperties=libertine_med)
    ax.set_xticks(range(len(qc_freqs)))
    ax.set_xticklabels([x[0].replace("_", "") for x in qc_freqs], fontproperties=libertine_small)
    ax.set_yticklabels(map(lambda x: "%.2f" % x, ax.get_yticks()), fontproperties=libertine_small)
    ax.legend(prop=libertine_small)

    fig.tight_layout()

    plt.savefig(experiment + "_freqs.png")
    plt.clf()


if __name__ == "__main__":
    plot_times("BST")
    plot_times("SORTED")
    plot_times("AVL")
    plot_times("STLC")

    plot_sizes("BST")
    plot_sizes("SORTED")
    plot_sizes("AVL")
    plot_sizes("STLC")

    plot_freqs("BST")
    plot_freqs("SORTED")
    plot_freqs("AVL")
    plot_freqs("STLC")
