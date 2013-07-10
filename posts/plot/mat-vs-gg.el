(deftitle "A Superficial Comparison of matplotlib vs ggplot2")

(defpara "Two plotting libraries in vogue nowadays are
Python's " (deflink "http://matplotlib.org" "matplotlib") " and
R's " (deflink "http://ggplot2.org" "ggplot2") ". Someone once
asked me why I use ggplot2 over matplotlib, and I remember
off-handedly replying that ggplot2's results just looked
better. But how true is this really?")

(defpara "To illustrate, I generated some basic examples below. I
used the default settings for each library, doing the equivalent
of a 'plot' followed by a 'save'.")

(defpara "For each pair of plots, ggplot2 is on the top, and matplotlib
on the bottom. You can click on the thumbnails for originals.")

(defpara "Histogram sampled from the standard normal (note the different default bin widths with ggplot2):")
(center
 (deflink "../files/r_hist.png" (defimg "../files/r_hist_thumb.png"))
 (deflink "../files/py_hist.png" (defimg "../files/py_hist_thumb.png")))

(defpara "Sine wave:"
  (center
   (deflink "../files/r_sine.png" (defimg "../files/r_sine_thumb.png"))
   (deflink "../files/py_sine.png" (defimg "../files/py_sine_thumb.png"))))

(defpara "Scatter plot sampled from U(0,1):"
  (center
   (deflink "../files/r_scatter.png" (defimg "../files/r_scatter_thumb.png"))
   (deflink "../files/py_scatter.png" (defimg "../files/py_scatter_thumb.png"))))

(defpara "(Code used to generate the above plots is " (deflink "https://github.com/ghalib/erblog/tree/master/posts/plot" "here") ")")

(defpara "Even from these very basic examples, you can see that
ggplot2's results have a noticeably more 'professional' feel to
them. The superior rendering, the grid background, and the automatic
labeling of axes all contribute to this.")

(defpara "If you are stuck with matplotlib and feel sad about it, fret
not. "(deflink "http://messymind.net/2012/07/making-matplotlib-look-like-ggplot/" "This
gentleman") " has code that makes matplotlib plots look more like
those of ggplot2. You can use it to, in his words, 'get some
really nice graphics going that you won't be ashamed to put in
your published material or presentation'.")