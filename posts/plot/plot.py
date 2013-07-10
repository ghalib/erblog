import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import pylab

def hist():
    x = np.random.randn(10000)
    pylab.hist(x)
    pylab.savefig("py_hist.png")
    pylab.close()
    
def sine():
    x = pylab.arange(0, 20, 0.1)
    s = pylab.sin(x)
    pylab.plot(x, s)
    pylab.savefig("py_sine.png")
    pylab.close()
    
def scatter():
    x = 0.9 * pylab.rand(30)
    y = 0.9 * pylab.rand(30)
    pylab.scatter(x, y)
    pylab.savefig("py_scatter.png")
    pylab.close()
    
