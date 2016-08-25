#1. Consider all the Fibonacci numbers whose values do not exceed four million. Find the sum of even-valued terms among them.
j = c(1,2)
i = j[length(j)-1] + j[length(j)]

while (i < 4000000) {
  j = append(j, i)
  i = j[length(j)-1] + j[length(j)]
}

sumevens = 0
for (num in j) {
  if (num %% 2 == 0) {
    sumevens = sumevens + num
  }
}
# D.D. - Can you combine these steps?  In other words, can you sum up the even Fibonacci numbers 
# "on-the-fly", without generating a large list of Fibonacci numbers?  It would be more efficient that way. 

#2. Write your own codes to implement the multiplication between a matrix and a vector.
matrixvector = function(mat, vec) {
  newvec = vector()
  for (i in 1:length(vec)) {
    newvec[i] = sum(mat[i,]*vec[i])
  }
  newvec
}

#3. Write your function that calculates the median absolute deviation (MAD) of a numeric vector.
madvec = function(vec) {
  newvec = vector()
  medvec = median(vec)
  for (i in 1:length(vec)) {
    newvec[i] = abs(vec[i] - medvec)
  }
  median(newvec)
}

#4. Suppose we have a character vector as follows: Names <- "John Andrew Thomas” Write some R code to obtain the following output: "John@gmail.com; A ndrew@gmail.com; Thomas@gmail.com"
gmail = function(names) {
  paste(unlist(strsplit(names, split = ' ')), '@gmail.com', collapse = '; ', sep = '')
}

#Write some R code to generate a vector with the following elements, without using loops. 
#"aa" "ba" "ca" "da" "ea" "ab" "bb" "cb" "db" "eb" "ac" "bc" "cc" "dc" "ec" "ad" "bd" "cd" "dd" "ed" "ae" "be" "ce" "de" "ee"
paste(c('a', 'b', 'c', 'd', 'e'),rep(c('a', 'b', 'c', 'd', 'e'), each = 5), sep = '')

# D.D. - Nice!