# North Star

modelminer is a framework for variable selection based on functional programming.

It should be able to take any model, any metric, any comparison, and select variables for it.

While the initial concept is about generalizing stepwise selection and including interactions and higher order terms, this package should ultimately enable any type of selection with more rigorous methods like LASSO (it wraps glmnet).

The interface should be friendly and incorporate NSE.
