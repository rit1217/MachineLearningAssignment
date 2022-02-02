library(KernelKnn)
library(elmNNRcpp)
install.packages("OpenImageR")
library(OpenImageR)
system("wget https://raw.githubusercontent.com/mlampros/DataSets/master/mnist.zip")             

mnist <- read.table(unz("mnist.zip", "mnist.csv"), nrows = 70000, header = T, 
                    
                    quote = "\"", sep = ",")

x = mnist[, -ncol(mnist)]

y = mnist[, ncol(mnist)]

# using system('wget..') on a linux OS 
#-------------------------------------

system("wget https://raw.githubusercontent.com/mlampros/DataSets/master/mnist.zip")             

mnist <- read.table(unz("mnist.zip", "mnist.csv"), nrows = 70000, header = T, 
                    
                    quote = "\"", sep = ",")

x = mnist[, -ncol(mnist)]

y = mnist[, ncol(mnist)] + 1


# use the hog-features as input data
#-----------------------------------

hog = OpenImageR::HOG_apply(x, cells = 6, orientations = 9, rows = 28, columns = 28, threads = 6)

y_expand = elmNNRcpp::onehot_encode(y - 1)


# 4-fold cross-validation
#------------------------

folds = KernelKnn:::class_folds(folds = 4, as.factor(y))
str(folds)

START = Sys.time()


fit = lapply(1:length(folds), function(x) {
  
  cat('\n'); cat('fold', x, 'starts ....', '\n')
  
  tmp_fit = elmNNRcpp::elm_train(as.matrix(hog[unlist(folds[-x]), ]), y_expand[unlist(folds[-x]), ], 
                                 
                                 nhid = 2500, actfun = 'relu', init_weights = 'uniform_negative',
                                 
                                 bias = TRUE, verbose = TRUE)
  
  cat('******************************************', '\n')
  
  tmp_fit
})

END = Sys.time()

END - START

# Time difference of 5.698552 mins


str(fit)


# predictions for 4-fold cross validation
#----------------------------------------

test_acc = unlist(lapply(1:length(fit), function(x) {
  
  pr_te = elmNNRcpp::elm_predict(fit[[x]], newdata = as.matrix(hog[folds[[x]], ]))
  
  pr_max_col = max.col(pr_te, ties.method = "random")
  
  y_true = max.col(y_expand[folds[[x]], ])
  
  mean(pr_max_col == y_true)
}))



test_acc

# [1] 0.9825143 0.9848571 0.9824571 0.9822857


cat('Accuracy ( Mnist data ) :', round(mean(test_acc) * 100, 2), '\n')

# Accuracy ( Mnist data ) : 98.3