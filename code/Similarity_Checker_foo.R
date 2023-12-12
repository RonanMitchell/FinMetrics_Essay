SimilarityChecker_foo <- function(df1, df2, column) {


    common_dates <- intersect(df1[[column]], df2[[column]])


    if (length(common_dates) == length(unique(df1[[column]])) &&
        length(common_dates) == length(unique(df2[[column]]))) {

        print("The columns are identical.")

    } else {

        print("The columns differ.")

    }

}