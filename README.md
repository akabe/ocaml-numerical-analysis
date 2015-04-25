# Stand-alone programs for numerical analysis in OCaml

Sometimes you need a small code (that has no dependency to a strange library or
a tool) for scientific computing. In this repository, we distribute such OCaml
programs under MIT license.

- [Multilayer neural network](multilayer-neural-network/):
  a neural network that has two or more layers can be used for nonlinear
  classification, regression, etc. in machine learning. In the past, multilayer
  neural networks are rarely applied for practical tasks because they have some
  problems such as [over-fitting](http://en.wikipedia.org/wiki/Overfitting) and
  [vanishing gradient](http://en.wikipedia.org/wiki/Vanishing_gradient_problem).
  After 2006, [Hinton](http://www.cs.toronto.edu/~hinton/) et al. proposed some
  epoch‐making approaches to solve the problems and accomplished surprisingly
  high performance. The newer techniques are also known as **deep learning**.
  This code is a very simple multilayer neural network, i.e., no
  state-of-the-art techniques like deep learning are **not** implemented. The
  following default setting is for classification. If you want to use this for
  regression, you should change the activation function of the output layer to a
  linear function.

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
