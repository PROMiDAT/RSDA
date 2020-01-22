## code to prepare `DATASET` dataset goes here
abalone <- SODAS.to.RSDA("data-raw/abalone.xml")
usethis::use_data(abalone, overwrite = T)

load("data-raw/Cardiological.rda")
class(Cardiological) <- "sym.data.table"
Cardiological <- RSDA:::to.v3(Cardiological)
usethis::use_data(Cardiological, overwrite = T)

ex1_db2so <- read.csv("data-raw/ex1_db2so.csv", row.names = 1)
usethis::use_data(ex1_db2so, overwrite = T)

load("data-raw/example1.rda")
class(example1) <- "sym.data.table"
example1 <- RSDA:::to.v3(example1)
usethis::use_data(example1, overwrite = T)

load("data-raw/example2.rda")
class(example2) <- "sym.data.table"
example2 <- RSDA:::to.v3(example2)
usethis::use_data(example2, overwrite = T)

load("data-raw/example3.rda")
class(example3) <- "sym.data.table"
example3 <- RSDA:::to.v3(example3)
usethis::use_data(example3, overwrite = T)

load("data-raw/example4.rda")
class(example4) <- "sym.data.table"
example4 <- RSDA:::to.v3(example4)
usethis::use_data(example4, overwrite = T)

load("data-raw/example5.rda")
class(example5) <- "sym.data.table"
example5 <- RSDA:::to.v3(example5)
usethis::use_data(example5, overwrite = T)

load("data-raw/example6.rda")
class(example6) <- "sym.data.table"
example6$sym.var.names <- c("F1","F2","F3","F4","F5","F6")
example6 <- RSDA:::to.v3(example6)
usethis::use_data(example6, overwrite = T)

load("data-raw/example7.rda")
class(example7) <- "sym.data.table"
example7 <- RSDA:::to.v3(example7)
usethis::use_data(example7, overwrite = T)

load("data-raw/ex_cfa1.rda")
class(ex_cfa1) <- "sym.data.table"
ex_cfa1 <- RSDA:::to.v3(ex_cfa1)
usethis::use_data(ex_cfa1, overwrite = T)

load("data-raw/ex_cfa2.rda")
class(ex_cfa2) <- "sym.data.table"
ex_cfa2 <- RSDA:::to.v3(ex_cfa2)
usethis::use_data(ex_cfa2, overwrite = T)

load("data-raw/ex_mcfa1.rda")
usethis::use_data(ex_mcfa1, overwrite = T)

load("data-raw/ex_mcfa2.rda")
usethis::use_data(ex_mcfa2, overwrite = T)

USCrime <- read.csv("data-raw/USCrime.csv", row.names = 1)
usethis::use_data(USCrime, overwrite = T)

ex_mcfa1 <- read.csv("data-raw/ex_mcfa1.csv", row.names = 1)
usethis::use_data(ex_mcfa1, overwrite = T)

load("data-raw/int_prost_train.rda")
class(int_prost_train) <- "sym.data.table"
int_prost_train <- RSDA:::to.v3(int_prost_train)
usethis::use_data(int_prost_train, overwrite = T)

load("data-raw/int_prost_test.rda")
class(int_prost_test) <- "sym.data.table"
int_prost_test <- RSDA:::to.v3(int_prost_test)
usethis::use_data(int_prost_test, overwrite = T)

load("data-raw/uscrime_int.rda")
class(uscrime_int) <- "sym.data.table"
uscrime_int <- RSDA:::to.v3(uscrime_int)
usethis::use_data(uscrime_int, overwrite = T)

load("data-raw/VeterinaryData.rda")
class(VeterinaryData) <- "sym.data.table"
VeterinaryData <- RSDA:::to.v3(VeterinaryData)
usethis::use_data(VeterinaryData, overwrite = T)
