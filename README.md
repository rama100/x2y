# x2y: An Alternative to the Correlation Coefficient That Works For Numeric *and* Categorical Variables

When starting to work with a new dataset, it is useful to quickly pinpoint which pairs of variables appear to be strongly related. It helps you spot data issues, make better modeling decisions, and ultimately arrive at better answers.

The [correlation coefficient](https://en.wikipedia.org/wiki/Correlation_coefficient) is used widely for this purpose but it has two shortcomings:
1. It can’t detect non-linear relationships
2. It doesn't work with categorical variables

We propose an alternative - the `x2y` metric - that remedies these shortcomings.

Please see this [article](https://rama100.github.io/lecture-notes/x2y.nb.html) for more detail on what it is and how it works, along with examples.
