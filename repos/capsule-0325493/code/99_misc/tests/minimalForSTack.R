dataSet <- structure(list(J1 = "foo", J2 = structure(0.1, .Dim = c(1L, 1L
))), .Names = c("J1", "J2"), row.names = 1L, class = "data.frame")
print(colnames(dataSet))
r <- is.na(dataSet)
print(colnames(r))