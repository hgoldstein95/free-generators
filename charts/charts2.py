import pandas as pd
from plotnine import ggplot, geom_bar, aes, stat_identity


def read_sizes(fname, method):
    with open(fname, "r") as f:
        return pd.DataFrame(data=[(method, i + 1, int(line))
                                  for (i, line) in enumerate(f.readlines())],
                            columns=["method", "size", "unique"])


def get_size_data(experiment):
    gradient = read_sizes("Grad_" + experiment + "_sizes.dat", "grad")
    rejection = read_sizes("QCRS_" + experiment + "_sizes.dat", "rej")
    return pd.concat([gradient, rejection], axis=0)


def plot_sizes(experiment):
    d = get_size_data(experiment)

    (ggplot(d, aes("size", "unique", fill="method")) +
     geom_bar(stat=stat_identity())).save("test.png")


if __name__ == "__main__":
    # plot_sizes("BST")
    # plot_sizes("SORTED")
    # plot_sizes("AVL")
    plot_sizes("STLC")
