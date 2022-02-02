library(elmNNRcpp)
library(OpenImageR)

mnist <- read.table(unz("mnist.zip", "mnist.csv"), nrows = 70000, header = T, 
                    
                    quote = "\"", sep = ",")

#create sampling
rows = sample(nrow(mnist), size = 5000, replace = TRUE)

x = mnist[rows,-ncol(mnist)]
y = mnist[rows, ncol(mnist)]

# use the hog-features as input data
hog = OpenImageR::HOG_apply(x, cells = 6, orientations = 9, rows = 28, columns = 28, threads = 4)
y_expand = elmNNRcpp::onehot_encode(y)

#train elm
model = elmNNRcpp::elm_train(as.matrix(hog[1:5000,]), y_expand, 
                               
                               nhid = 100, actfun = 'relu', init_weights = 'uniform_negative',
        
                               bias = TRUE, verbose = TRUE)

#create testing dataset
new_rows = sample(nrow(mnist), size = 5000, replace = TRUE)

x2 = mnist[new_rows,-ncol(mnist)]
y2 = mnist[new_rows, ncol(mnist)]

hog2 = OpenImageR::HOG_apply(x2, cells = 6, orientations = 9, rows = 28, columns = 28, threads = 4)
y_expand2 = elmNNRcpp::onehot_encode(y2)

#predict the dataset
pr_te = elmNNRcpp::elm_predict(model, newdata = as.matrix(hog2))

pr_max_col = max.col(pr_te, ties.method = "random")

y_true = max.col(y_expand2)

test_acc = mean(pr_max_col == y_true)

cat('Accuracy :', test_acc, '\n')

