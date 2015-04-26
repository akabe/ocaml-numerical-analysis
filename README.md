# Stand-alone programs for numerical analysis in OCaml

Sometimes you need a small code (that has no dependency to a strange library or
a tool) for scientific computing. In this repository, we distribute such OCaml
programs under MIT license (the copyright of each data set belongs to the maker
of the data).

- [Fast Fourier transform](fft/):
  This is an implementation of radix-2
  [Cooley-Tukey fast Fourier transform (FFT) algorithm](http://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm),
  the most famous method in FFT algorithms. The naive computation of discrete
  Fourier transform according to the definition takes O(n^2) time, but this
  algorithm takes O(n log n) time. Fourier transform is frequently used for
  signal analysis, data compression, etc.

- [Durand-Kerner-Aberth method](durand-kerner-aberth/):
  [Durand-Kerner method](http://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method)
  is an algorithm to find all (complex) roots of a given polynominal at the same
  time, and [Aberth method](http://en.wikipedia.org/wiki/Aberth_method) is an
  approach to compute the initial values for Durand-Kerner method.

- [Autocorrelation & Levinson-Durbin recursion](levinson-durbin/):
  [Levinson-Durbin recursion](http://en.wikipedia.org/wiki/Levinson_recursion)
  is an algorithm to compute AR coefficients of
  [autoregressive (AR) model](http://en.wikipedia.org/wiki/Autoregressive_model).
  The most well-known application of AR model is
  [linear predictive coding (LPC)](http://en.wikipedia.org/wiki/Linear_predictive_coding),
  a classic analysis/coding/compression approach for voice. We decompose
  input voice into *glottal source* (buzz-like sound) and *vocal tract filter
  characteristics* (filter coefficients) by using Levinson-Durbin algorithm,
  and analyze or encode the two kinds of sound by different ways.
  LPC vocoder (voice coder) is applied to
  [FS-1015](http://en.wikipedia.org/wiki/FS-1015) (secure telephony speech
  encoding), [Shorten](http://en.wikipedia.org/wiki/Shorten_(file_format)),
  [MPEG-4 ALS](http://en.wikipedia.org/wiki/MPEG-4_ALS),
  [FLAC](http://en.wikipedia.org/wiki/FLAC) audio codec, etc. This program
  computes AR coefficients from time-domain sound and outputs them.

  - Compilation: `ocamlopt dataset.ml levinson.ml`
  - Data set: Japanese vowel sound /a/, /i/, /u/, /e/, /o/
    (http://www.gavo.t.u-tokyo.ac.jp/~mine/B3enshu2001/samples.html)
  - AR order: 20

- [Naive multilayer neural network](neural-network/naive-multilayer):
  a neural network that has two or more layers can be used for nonlinear
  classification, regression, etc. in machine learning. This code is a very
  simple implementation of multilayer neural network. This neural network tends
  to fall into over-fitting. (In the past, multilayer neural networks are rarely
  applied for practical tasks because they have some problems such as
  [over-fitting](http://en.wikipedia.org/wiki/Overfitting) and
  [vanishing gradient](http://en.wikipedia.org/wiki/Vanishing_gradient_problem).
  After 2006, [Hinton](http://www.cs.toronto.edu/~hinton/) et al. proposed some
  epoch‐making approaches to solve the problems and accomplished surprisingly
  high performance. The newer techniques are also known as *deep learning*.)
  The following default setting is for classification. If you want to use this
  for regression, you should change the activation function of the output layer
  to a linear function, and the error function to sum of squared errors.

  - Compilation: `ocamlopt dataset.ml neuralNetwork.ml`
  - Data set: [Ionosphere (UCI Machine Learning Repository)](https://archive.ics.uci.edu/ml/datasets/Ionosphere)
    (\#features = 34, \#classes = 2)
  - Training: error backpropagation
    [[Rumelhard et al., 1986]](http://dl.acm.org/citation.cfm?id=104293) +
    stochastic gradient descent (with a constant learning rate)
  - Regularization: none
  - Error function: cross-entropy
  - Layers: 4 layers + the input layer (all neurons in each layer are connected
    with all neurons in the lower layer.
  - The 1st hidden layer: 10 unis, activation function = tanh
  - The 2nd hidden layer: 5 units, activation function = tanh
  - The output layer: 2 units (binary classification, 1-of-K coding),
    activation function = softmax
