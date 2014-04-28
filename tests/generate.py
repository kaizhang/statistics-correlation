import random
from scipy.stats import kendalltau

out = open("result.txt", 'w')
data = open("data.txt", 'w')

for _ in range(200):
    a = [random.gauss(10, 100) for _ in range(100)]
    b = [random.gauss(10, 1000) for _ in range(100)]
    tau, p = kendalltau(a, b)
    print('\t'.join(map(str, a)), file=data)
    print('\t'.join(map(str, b)), file=data)

    print("{0:0.5f}".format(tau), file=out)
