

Animal <- setRefClass("Animal",
                      fields = list(name = "character", age = "numeric"),
                      methods = list(
                        initialize = function(name , age){
                          .self$name <<- name
                          .self$age <<- age
                          cat("Animal", name, "created \n")
                          cat(name, "is:" ,age, "years old \n")
                        }

                      )

                      )


ok <- Animal("moo",4)


ok$age


data(iris)

X <- model.matrix(Sepal.Length ~ Sepal.Width, iris)
print(X)

dim(X)
attributes(X)
